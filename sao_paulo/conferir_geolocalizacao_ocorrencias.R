#conferir geolocalização das ocorrencias

library(arrow)
library(tidyverse)
library(geobr)
library(geocodebr)
library(sf)

df <- read_parquet("dados/SPDadosCriminais_jan_ago2026.parquet")
names(df) <- gsub(".", "_", names(df), fixed = TRUE)

# ---------------------------------------------------------------------------
# 1. Limpeza de LATITUDE/LONGITUDE (vem como texto) e correcao de coordenadas
#    malformadas (ponto decimal ausente, ex: "-235286" em vez de "-23.5286")
# ---------------------------------------------------------------------------

fix_coord <- function(x_chr) {
  x_num <- suppressWarnings(as.numeric(x_chr))
  sem_ponto <- !grepl(".", x_chr, fixed = TRUE) & !is.na(x_num) & x_num != 0
  sinal <- ifelse(startsWith(x_chr, "-"), "-", "")
  digitos <- sub("^-", "", x_chr)
  intpart <- substr(digitos, 1, 2)
  decpart <- substr(digitos, 3, nchar(digitos))
  corrigido <- suppressWarnings(as.numeric(paste0(sinal, intpart, ".", decpart)))
  ifelse(sem_ponto, corrigido, x_num)
}

lat_num <- fix_coord(df$LATITUDE)
lon_num <- fix_coord(df$LONGITUDE)

bbox_sp <- lat_num > -26 & lat_num < -19 & lon_num > -54 & lon_num < -44
valido <- !is.na(lat_num) & lat_num != 0 & !is.na(lon_num) & lon_num != 0 & bbox_sp

# coordenada nao-zero/nao-NA mas fora da faixa plausivel mesmo apos a correcao
# (nao seguiu o padrao do bug de ponto decimal) -> precisa geocodificar por
# endereco e fica marcada para revisao manual
coord_suspeita <- !is.na(lat_num) & lat_num != 0 & !is.na(lon_num) & lon_num != 0 & !bbox_sp

sigilo <- grepl("VEDA", df$LOGRADOURO, ignore.case = TRUE)

cat("== Diagnostico de qualidade ==\n")
cat("Total:", nrow(df), "\n")
cat("Validos:", sum(valido), sprintf("(%.1f%%)\n", 100 * mean(valido)))
cat("Sigilosos (sem endereco disponivel):", sum(sigilo), sprintf("(%.1f%%)\n", 100 * mean(sigilo)))
cat("Geocodificaveis por endereco:", sum(!sigilo & !valido), "\n")
cat("Coordenadas suspeitas (fora de faixa, nao corrigiveis automaticamente):", sum(coord_suspeita), "\n")

# ---------------------------------------------------------------------------
# 2. Geocodificacao via geocodebr para os registros com endereco disponivel
#    (sem sigilo). Geocodifica apenas enderecos DISTINTOS para economizar
#    tempo e depois faz o join de volta.
# ---------------------------------------------------------------------------

geocodavel <- !sigilo & !valido

enderecos_distintos <- df[geocodavel, ] %>%
  mutate(
    .id_endereco = row_number(),
    ESTADO = "SP"
  ) %>%
  distinct(LOGRADOURO, NUMERO_LOGRADOURO, BAIRRO, COD_IBGE, ESTADO, .keep_all = FALSE)

cat("\nEnderecos distintos a geocodificar:", nrow(enderecos_distintos), "\n")

resultado_geocode <- geocodebr::geocode(
  enderecos = enderecos_distintos,
  campos_endereco = definir_campos(
    estado      = "ESTADO",
    municipio   = "COD_IBGE",
    logradouro  = "LOGRADOURO",
    numero      = "NUMERO_LOGRADOURO",
    localidade  = "BAIRRO"
  ),
  resultado_completo = FALSE,
  verboso = TRUE,
  n_cores = 2
) %>%
  select(LOGRADOURO, NUMERO_LOGRADOURO, BAIRRO, COD_IBGE,
         lat_geocode = lat, lon_geocode = lon, precisao_geocode = precisao)

cat("\nTaxa de sucesso geocodebr:",
    sprintf("%.1f%%\n", 100 * mean(!is.na(resultado_geocode$lat_geocode))))
cat("Distribuicao de precisao:\n")
print(table(resultado_geocode$precisao_geocode, useNA = "ifany"))

# ---------------------------------------------------------------------------
# 3. Para os registros sigilosos: aproximacao pelo medoide dos pontos validos
#    da mesma circunscricao policial (NOME_DELEGACIA_CIRCUNSCRICAO). Nao se
#    usa centroide de bairro para nao recriar o risco de reidentificacao que
#    o sigilo pretende evitar; e nao se usa poligono administrativo pois nao
#    existe shapefile publico de circunscricao de delegacia.
# ---------------------------------------------------------------------------

pontos_validos <- df %>%
  mutate(lat_ok = lat_num, lon_ok = lon_num) %>%
  filter(valido) %>%
  select(NOME_DELEGACIA_CIRCUNSCRICAO, lat_ok, lon_ok)

medoide_por_delegacia <- pontos_validos %>%
  group_by(NOME_DELEGACIA_CIRCUNSCRICAO) %>%
  mutate(
    lat_centro = mean(lat_ok), lon_centro = mean(lon_ok),
    dist2 = (lat_ok - lat_centro)^2 + (lon_ok - lon_centro)^2
  ) %>%
  slice_min(dist2, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(NOME_DELEGACIA_CIRCUNSCRICAO, lat_medoide = lat_ok, lon_medoide = lon_ok)

# fallback: delegacias sem nenhum ponto valido proprio -> centroide do
# municipio via geobr (ponto garantido dentro do poligono)
delegacias_sigilo <- df %>% filter(sigilo) %>%
  distinct(NOME_DELEGACIA_CIRCUNSCRICAO, COD_IBGE)

sem_ancora <- anti_join(delegacias_sigilo, medoide_por_delegacia,
                         by = "NOME_DELEGACIA_CIRCUNSCRICAO")

if (nrow(sem_ancora) > 0) {
  cat("\nDelegacias sem ponto proprio, usando centroide municipal via geobr:\n")
  print(sem_ancora)

  municipios_fallback <- geobr::read_municipality(code_muni = sem_ancora$COD_IBGE, year = 2022, showProgress = FALSE)

  centroides_fallback <- municipios_fallback %>%
    mutate(pt = sf::st_point_on_surface(geometry)) %>%
    mutate(lon_medoide = sf::st_coordinates(pt)[, 1],
           lat_medoide = sf::st_coordinates(pt)[, 2]) %>%
    st_drop_geometry() %>%
    select(COD_IBGE = code_muni, lat_medoide, lon_medoide) %>%
    left_join(sem_ancora, ., by = "COD_IBGE") %>%
    select(NOME_DELEGACIA_CIRCUNSCRICAO, lat_medoide, lon_medoide)

  medoide_por_delegacia <- bind_rows(medoide_por_delegacia, centroides_fallback)
}

# ---------------------------------------------------------------------------
# 4. Monta o dataframe final consolidado
# ---------------------------------------------------------------------------

df_geo <- df %>%
  mutate(
    LATITUDE_ORIGINAL  = lat_num,
    LONGITUDE_ORIGINAL = lon_num,
    sigilo_legal   = sigilo,
    revisar_manual = coord_suspeita
  ) %>%
  left_join(resultado_geocode,
            by = c("LOGRADOURO", "NUMERO_LOGRADOURO", "BAIRRO", "COD_IBGE")) %>%
  left_join(medoide_por_delegacia, by = "NOME_DELEGACIA_CIRCUNSCRICAO") %>%
  mutate(
    LATITUDE_FINAL = case_when(
      valido                        ~ LATITUDE_ORIGINAL,
      sigilo_legal                  ~ lat_medoide,
      !is.na(lat_geocode)           ~ lat_geocode,
      TRUE                          ~ NA_real_
    ),
    LONGITUDE_FINAL = case_when(
      valido                        ~ LONGITUDE_ORIGINAL,
      sigilo_legal                  ~ lon_medoide,
      !is.na(lon_geocode)           ~ lon_geocode,
      TRUE                          ~ NA_real_
    ),
    fonte_geo = case_when(
      valido                        ~ "original",
      sigilo_legal                  ~ "aproximado_circunscricao_medoide",
      !is.na(lat_geocode)           ~ "geocodebr",
      TRUE                          ~ "nao_geocodificado"
    ),
    precisao_geo = case_when(
      valido                        ~ "exata_original",
      sigilo_legal                  ~ "circunscricao_policial",
      !is.na(precisao_geocode)      ~ precisao_geocode,
      TRUE                          ~ NA_character_
    )
  ) %>%
  select(-lat_geocode, -lon_geocode, -precisao_geocode, -lat_medoide, -lon_medoide)

cat("\n== Resumo final df_geo ==\n")
print(table(df_geo$fonte_geo, useNA = "ifany"))
cat("\nCobertura final (com coordenada valida):",
    sprintf("%.1f%%\n", 100 * mean(!is.na(df_geo$LATITUDE_FINAL))))

write_parquet(df_geo, "dados/SPDadosCriminais_jan_ago2026_geocodificado.parquet")
cat("\nSalvo em dados/SPDadosCriminais_jan_ago2026_geocodificado.parquet\n")
