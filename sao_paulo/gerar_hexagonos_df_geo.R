# ============================================================================
# Grade hexagonal H3 — ocorrências criminais geolocalizadas (df_geo)
# ============================================================================
# Pré-requisito: dados/SPDadosCriminais_jan_ago2026_geocodificado.parquet já
# existe (gerado por conferir_geolocalizacao_ocorrencias.R) — é o df_geo:
# 732.808 ocorrências, 100% com coordenada final (LATITUDE_FINAL/
# LONGITUDE_FINAL), mas com proveniência e precisão desiguais (coluna
# precisao_geo): da coordenada exata original até a aproximação por
# circunscrição policial dos registros sob sigilo legal.
#
# Resoluções H3 geradas: 4 (~1 770 km²/hexágono), 6 (~36 km²), 8 (~0,74 km²)
# e 9 (~0,11 km²).
#
# -----------------------------------------------------------------------
# Por que a contagem direta por hexágono não basta aqui
# -----------------------------------------------------------------------
# 127.040 registros (17,3% da base) não têm endereço — são sigilosos — e
# receberam uma coordenada aproximada: o medoide dos pontos já válidos da
# mesma circunscrição de delegacia (ver conferir_geolocalizacao_ocorrencias.R).
# Isso significa que, em cada circunscrição, TODOS os registros sigilosos
# compartilham exatamente a mesma coordenada. Em resoluções grossas (4, 6)
# isso não é um problema — a circunscrição inteira cabe dentro de um único
# hexágono de qualquer forma. Mas em resoluções finas (8, 9), onde um
# hexágono cobre uma fração pequena da área da circunscrição, contar esses
# registros diretamente no hexágono do medoide criaria um pico artificial
# de ocorrências ali e zero nos hexágonos vizinhos — justamente onde boa
# parte desses crimes provavelmente aconteceu. O mesmo problema, em menor
# escala, afeta os 2.354 registros geocodificados só até o nível de
# município e os 3.253 só até o nível de bairro (precisao_geo).
#
# Em vez disso, cada ocorrência "areal" (sem ponto confiável) é distribuída
# FRACIONADAMENTE entre os hexágonos onde já existem ocorrências com
# geolocalização confiável na MESMA unidade administrativa (bairro,
# município ou circunscrição, conforme o caso) — proporcionalmente ao
# padrão espacial já observado ali. Isso preserva integralmente a
# informação de volume (o total de ocorrências por hexágono continua
# somando 732.808 em qualquer resolução — conferido abaixo) sem fabricar
# uma falsa precisão pontual para dados que nunca tiveram um ponto exato.
# Quando uma unidade não tem nenhum ponto confiável para servir de
# referência (ex.: DEL.POL.ITU, DEL.POL.INÚBIA PAULISTA), mantém-se o
# ponto-âncora original — é o único dado espacial disponível ali.
# ============================================================================

library(sf)
library(dplyr)
library(h3jsr)
library(arrow)

df_geo <- read_parquet("dados/SPDadosCriminais_jan_ago2026_geocodificado.parquet")

stopifnot(
  "df_geo deveria ter 100% de cobertura de coordenada final" =
    sum(is.na(df_geo$LATITUDE_FINAL) | is.na(df_geo$LONGITUDE_FINAL)) == 0
)

pontos <- st_as_sf(
  df_geo,
  coords = c("LONGITUDE_FINAL", "LATITUDE_FINAL"),
  crs = 4326,
  remove = FALSE
)

resolucoes_h3 <- c(4, 6, 8, 9)

# Indexa cada ocorrência na célula H3 que contém sua coordenada final, em
# cada resolução (atribuição direta — usada como base tanto para a tabela
# tabular quanto como "ponto confiável" na redistribuição abaixo).
for (res in resolucoes_h3) {
  col <- paste0("h3_res", res)
  pontos[[col]] <- point_to_cell(pontos, res = res, simple = TRUE)
}

df_geo <- st_drop_geometry(pontos)

# Tiers de precisão com ponto confiável o suficiente para contar
# diretamente no seu próprio hexágono, mesmo nas resoluções mais finas.
TIERS_PONTUAIS <- c("exata_original", "numero", "numero_aproximado", "logradouro")

# Tiers "areais": a coordenada representa uma unidade administrativa, não
# um local específico. Cada um é redistribuído usando o padrão espacial
# dos pontos confiáveis da mesma unidade (coluna(s) chave indicada).
TIERS_AREAIS <- list(
  localidade             = c("BAIRRO", "NOME_MUNICIPIO"),
  municipio              = "COD_IBGE",
  circunscricao_policial = "NOME_DELEGACIA_CIRCUNSCRICAO"
)

#' Gera a grade hexagonal H3 agregada para uma resolução, redistribuindo os
#' registros sem ponto confiável conforme descrito no cabeçalho do script.
#'
#' @param df tibble com a coluna de índice H3 já calculada (`col_h3`) e as
#'   colunas `precisao_geo` + as colunas-chave de TIERS_AREAIS.
#' @param col_h3 nome da coluna com o índice H3 pré-calculado nesta resolução.
#' @return sf de polígonos hexagonais com `ocorrencias` (total) e o detalhamento
#'   `ocorrencias_diretas` + `estimadas_<tier>` por fonte.
gerar_grade_hex <- function(df, col_h3) {
  dados <- df
  dados$h3_address <- dados[[col_h3]]

  pontuais <- filter(dados, precisao_geo %in% TIERS_PONTUAIS)
  contagem_direta <- count(pontuais, h3_address, name = "ocorrencias_diretas")

  tabelas_areais <- lapply(names(TIERS_AREAIS), function(tier) {
    key_cols <- TIERS_AREAIS[[tier]]

    # padrão espacial observado (pontos confiáveis) dentro de cada unidade
    distrib_ref <- pontuais |>
      count(across(all_of(key_cols)), h3_address, name = "n_ref") |>
      group_by(across(all_of(key_cols))) |>
      mutate(peso = n_ref / sum(n_ref)) |>
      ungroup() |>
      select(all_of(key_cols), h3_address, peso)

    total_por_unidade <- dados |>
      filter(precisao_geo == tier) |>
      count(across(all_of(key_cols)), h3_address, name = "n_unidade") |>
      group_by(across(all_of(key_cols))) |>
      summarise(n_total = sum(n_unidade), h3_ancora = first(h3_address), .groups = "drop")

    # unidades com padrão de referência -> distribui proporcionalmente
    com_referencia <- total_por_unidade |>
      inner_join(distrib_ref, by = key_cols) |>
      mutate(peso_final = n_total * peso) |>
      count(h3_address, wt = peso_final, name = "estimadas")

    # unidades sem nenhum ponto confiável -> mantém o ponto-âncora (fallback)
    sem_referencia <- total_por_unidade |>
      anti_join(distinct(distrib_ref, across(all_of(key_cols))), by = key_cols) |>
      count(h3_ancora, wt = n_total, name = "estimadas") |>
      rename(h3_address = h3_ancora)

    bind_rows(com_referencia, sem_referencia) |>
      group_by(h3_address) |>
      summarise(estimadas = sum(estimadas), .groups = "drop") |>
      rename(!!paste0("estimadas_", tier) := estimadas)
  })

  grade <- contagem_direta
  for (tabela in tabelas_areais) grade <- full_join(grade, tabela, by = "h3_address")

  cols_num <- setdiff(names(grade), "h3_address")
  grade <- grade |> mutate(across(all_of(cols_num), \(x) coalesce(x, 0)))
  grade$ocorrencias <- rowSums(grade[cols_num])

  cell_to_polygon(grade$h3_address, simple = FALSE) |>
    left_join(grade, by = "h3_address")
}

grades_hex <- setNames(
  lapply(resolucoes_h3, function(res) gerar_grade_hex(df_geo, paste0("h3_res", res))),
  paste0("r", resolucoes_h3)
)

# Checagem de conservação: nenhuma ocorrência pode ser perdida nem
# duplicada pela redistribuição — a soma de `ocorrencias` em qualquer
# resolução tem que bater exatamente com o total de linhas de df_geo.
for (res in resolucoes_h3) {
  grade <- grades_hex[[paste0("r", res)]]
  total <- sum(grade$ocorrencias)
  message(sprintf(
    "Resolução %d: %d hexágonos ocupados, %.1f ocorrências somadas (esperado %d)",
    res, nrow(grade), total, nrow(df_geo)
  ))
  stopifnot(
    "Soma de ocorrencias por hexagono nao bate com o total de df_geo (vazamento na redistribuicao)" =
      abs(total - nrow(df_geo)) < 1e-6
  )
}

grade_hex_r4 <- grades_hex[["r4"]]
grade_hex_r6 <- grades_hex[["r6"]]
grade_hex_r8 <- grades_hex[["r8"]]
grade_hex_r9 <- grades_hex[["r9"]]

# Salva a grade agregada de cada resolução: .rds preserva a geometria sf
# (para mapas); .parquet guarda a versão tabular (sem geometria) para
# consumo fora do R.
for (res in resolucoes_h3) {
  grade <- grades_hex[[paste0("r", res)]]
  saveRDS(grade, sprintf("dados/hex_r%d_df_geo.rds", res))
  write_parquet(st_drop_geometry(grade), sprintf("dados/hex_r%d_df_geo.parquet", res))
}
#remover duplicatas
df_geo <- df_geo |> distinct()
# Salva também a tabela de ocorrências (nível linha) com o índice H3 direto
# de cada resolução anexado — útil para outros cruzamentos que não a grade
# agregada em si.
write_parquet(df_geo, "dados/SPDadosCriminais_jan_ago2026_geo_hex.parquet")
