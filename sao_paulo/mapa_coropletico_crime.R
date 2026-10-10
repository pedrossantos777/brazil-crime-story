# ============================================================================
# Mapa coroplético por crime — escala Jenks, resolução H3 escolhida
# ============================================================================
# Pré-requisito: dados/SPDadosCriminais_jan_ago2026_geo_hex.parquet já existe
# (gerado por gerar_hexagonos_df_geo.R) — é o df_geo com os índices H3
# diretos (h3_res4/6/8/9) já calculados por linha.
#
# Fornece mapa_coropletico_crime(), que recebe o crime (coluna
# NATUREZA_APURADA), a resolução H3 e, opcionalmente, um município —
# sem município, o mapa é do estado inteiro.
#
# -----------------------------------------------------------------------
# Por que filtrar por crime não é só um `filter()` antes de contar hexágono
# -----------------------------------------------------------------------
# Crimes sob sigilo legal (estupro, estupro de vulnerável etc. — ver
# conferir_geolocalizacao_ocorrencias.R) têm quase 100% dos seus próprios
# registros SEM nenhum ponto confiável: a coordenada de todos eles já é a
# aproximação por circunscrição de delegacia (precisao_geo ==
# "circunscricao_policial"). Se a redistribuição espacial (ver
# gerar_hexagonos_df_geo.R) fosse recalculada usando só os registros do
# crime escolhido, não sobraria nenhum ponto confiável para servir de
# referência em boa parte das circunscrições — e o mapa de estupro, por
# exemplo, voltaria a ser um conjunto de picos artificiais no hexágono do
# medoide de cada delegacia.
#
# Por isso o padrão espacial de referência (quais hexágonos concentram
# ocorrências dentro de cada circunscrição/bairro/município) é sempre
# calculado usando TODOS os crimes do município selecionado (ou do estado
# inteiro, no caso padrão) — presume-se que a distribuição geográfica da
# criminalidade em geral dentro de uma mesma jurisdição é um proxy razoável
# de onde um crime específico e sigiloso também se concentra. Só a
# CONTAGEM final (quantas ocorrências) é filtrada para o crime escolhido.
# ============================================================================

library(sf)
library(dplyr)
library(ggplot2)
library(h3jsr)
library(arrow)
library(classInt)
library(stringi)
library(geobr)

RESOLUCOES_VALIDAS <- c(4, 6, 8, 9)

TIERS_PONTUAIS <- c("exata_original", "numero", "numero_aproximado", "logradouro")

TIERS_AREAIS <- list(
  localidade             = c("BAIRRO", "NOME_MUNICIPIO"),
  municipio              = "COD_IBGE",
  circunscricao_policial = c("NOME_DELEGACIA_CIRCUNSCRICAO")
)

#' Normaliza texto para comparação tolerante a acento/maiúscula/pontuação
#' (necessário porque a SSP-SP abrevia município de forma inconsistente,
#' ex. "S.PAULO" em vez de "São Paulo" — ver README.md).
normalizar_texto <- function(x) {
  x |> stri_trans_general("Latin-ASCII") |> toupper() |> gsub("[^A-Z0-9]", "", x = _)
}

#' Encontra o valor exato de uma coluna que corresponde (com tolerância a
#' acento/pontuação, e por último por distância aproximada) ao texto
#' digitado pelo usuário. Para com mensagem de erro e sugestões se não achar.
#'
#' @param abreviar_sao Se `TRUE`, tenta também a forma abreviada que a
#'   SSP-SP usa para município ("São Paulo" -> "S.PAULO", "Santo André" ->
#'   "S.ANDRE") antes de recorrer à distância aproximada — sem isso, o
#'   fuzzy match de "SAO PAULO" tende a empatar com várias cidades do
#'   interior terminadas em "...PAULISTA" antes de achar "S.PAULO".
resolver_valor <- function(valor_usuario, valores_possiveis, rotulo, abreviar_sao = FALSE) {
  alvo <- normalizar_texto(valor_usuario)
  candidatos_norm <- normalizar_texto(valores_possiveis)

  exato <- valores_possiveis[candidatos_norm == alvo]
  if (length(exato) >= 1) return(exato[1])

  if (abreviar_sao) {
    texto_upper <- toupper(stri_trans_general(valor_usuario, "Latin-ASCII"))
    alvo_abrev <- normalizar_texto(sub("^(SAO|SANTO|SANTA)\\s+", "S.", texto_upper))
    exato_abrev <- valores_possiveis[candidatos_norm == alvo_abrev]
    if (length(exato_abrev) >= 1) return(exato_abrev[1])
  }

  aprox_idx <- agrep(alvo, candidatos_norm, max.distance = 0.3)
  if (length(aprox_idx) == 1) return(valores_possiveis[aprox_idx])

  sugestoes <- if (length(aprox_idx) > 1) valores_possiveis[aprox_idx] else head(sort(valores_possiveis), 10)
  stop(sprintf(
    "%s '%s' não encontrado. Candidatos próximos: %s",
    rotulo, valor_usuario, paste(sugestoes, collapse = ", ")
  ), call. = FALSE)
}

#' Gera o mapa coroplético hexagonal (H3) de um crime, na escala de
#' classificação Jenks (quebras naturais).
#'
#' @param crime Nome do crime a mapear — comparado (com tolerância a
#'   acento/maiúscula) contra a coluna `NATUREZA_APURADA` de df_geo. Use
#'   `sort(unique(df_geo$NATUREZA_APURADA))` para ver as opções.
#' @param resolucao Resolução H3: 4 (~1.770 km²/hexágono), 6 (~36 km²),
#'   8 (~0,74 km²) ou 9 (~0,11 km²).
#' @param municipio Opcional. Nome do município (coluna `NOME_MUNICIPIO`)
#'   para restringir o mapa. Se `NULL` (padrão), plota o estado inteiro.
#'   Quando informado, sobrepõe também os limites de bairro via
#'   `geobr::read_weighting_area()` (áreas de ponderação do censo) — usado
#'   no lugar de `geobr::read_neighborhood()` porque este último não cobre
#'   a capital. Na cidade de São Paulo essas áreas vêm nomeadas por bairro
#'   (ex. "Sé", "Bixiga"); em outros municípios podem ser agregados maiores.
#' @param n_classes Número de classes da escala Jenks (padrão 5).
#' @param df Dataframe de origem (padrão: lê
#'   dados/SPDadosCriminais_jan_ago2026_geo_hex.parquet).
#' @return Um objeto ggplot (não é impresso nem salvo automaticamente).
mapa_coropletico_crime <- function(crime,
                                    resolucao,
                                    municipio = NULL,
                                    n_classes = 5,
                                    df = NULL) {

  stopifnot(
    "resolucao precisa ser uma de 4, 6, 8 ou 9" = resolucao %in% RESOLUCOES_VALIDAS
  )

  if (is.null(municipio) && resolucao %in% c(8, 9)) {
    message(
      "Aviso: resolucao ", resolucao, " sem municipio cobre o estado inteiro com ",
      "hexagonos muito pequenos (", if (resolucao == 8) "~0,74 km2" else "~0,11 km2",
      " cada) — o mapa pode parecer vazio/em branco mesmo estando correto, porque ",
      "cada hexagono fica proximo de ilegivel nessa escala, principalmente para ",
      "crimes raros. Para essas resolucoes, informe `municipio` para um mapa legivel, ",
      "ou use resolucao 4/6 para o estado inteiro."
    )
  }

  if (is.null(df)) {
    df <- read_parquet("dados/SPDadosCriminais_jan_ago2026_geo_hex.parquet")
  }

  col_h3 <- paste0("h3_res", resolucao)
  stopifnot(
    "df não tem a coluna de índice H3 pedida — rode gerar_hexagonos_df_geo.R primeiro" =
      col_h3 %in% names(df)
  )

  crime_resolvido <- resolver_valor(crime, unique(df$NATUREZA_APURADA), "Crime")

  poligono_municipio <- NULL
  poligono_bairros <- NULL
  base <- df
  if (!is.null(municipio)) {
    municipio_resolvido <- resolver_valor(municipio, unique(df$NOME_MUNICIPIO), "Município", abreviar_sao = TRUE)
    base <- filter(df, NOME_MUNICIPIO == municipio_resolvido)
    cod_ibge_municipio <- base$COD_IBGE[1]
    poligono_municipio <- tryCatch(
      geobr::read_municipality(code_muni = cod_ibge_municipio, year = 2022, showProgress = FALSE),
      error = function(e) NULL
    )
    # geobr::read_neighborhood() não cobre a capital (só municípios menores
    # do estado) — usa as áreas de ponderação do censo como aproximação de
    # bairro: no caso de São Paulo capital elas vêm nomeadas por bairro
    # (ex. "Sé", "Bom Retiro", "Bixiga"), diferente de outros municípios
    # onde podem ser agregados maiores sem correspondência direta a bairro.
    poligono_bairros <- tryCatch(
      geobr::read_weighting_area(year = 2022, code_weighting = cod_ibge_municipio, showProgress = FALSE),
      error = function(e) NULL
    )
  }

  alvo <- filter(base, NATUREZA_APURADA == crime_resolvido)
  if (nrow(alvo) == 0) {
    stop(sprintf(
      "Nenhuma ocorrência de '%s'%s.", crime_resolvido,
      if (!is.null(municipio)) paste0(" em ", municipio) else " no estado"
    ), call. = FALSE)
  }

  # padrão espacial de referência: TODOS os crimes da mesma base (município
  # escolhido, ou estado inteiro) — ver explicação no cabeçalho do script.
  base$h3_address <- base[[col_h3]]
  alvo$h3_address <- alvo[[col_h3]]

  pontuais_ref <- filter(base, precisao_geo %in% TIERS_PONTUAIS)
  pontuais_alvo <- filter(alvo, precisao_geo %in% TIERS_PONTUAIS)
  contagem_direta <- count(pontuais_alvo, h3_address, name = "ocorrencias_diretas")

  tabelas_areais <- lapply(names(TIERS_AREAIS), function(tier) {
    key_cols <- TIERS_AREAIS[[tier]]

    distrib_ref <- pontuais_ref |>
      count(across(all_of(key_cols)), h3_address, name = "n_ref") |>
      group_by(across(all_of(key_cols))) |>
      mutate(peso = n_ref / sum(n_ref)) |>
      ungroup() |>
      select(all_of(key_cols), h3_address, peso)

    total_por_unidade <- alvo |>
      filter(precisao_geo == tier) |>
      count(across(all_of(key_cols)), h3_address, name = "n_unidade") |>
      group_by(across(all_of(key_cols))) |>
      summarise(n_total = sum(n_unidade), h3_ancora = first(h3_address), .groups = "drop")

    com_referencia <- total_por_unidade |>
      inner_join(distrib_ref, by = key_cols) |>
      mutate(peso_final = n_total * peso) |>
      count(h3_address, wt = peso_final, name = "estimadas")

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
  # A redistribuição proporcional por peso espacial (ver cabeçalho do script)
  # produz estimativas fracionárias; arredonda para inteiro aqui, antes da
  # classificação Jenks, para que as faixas da legenda sejam valores
  # inteiros (contagem de ocorrências), e não frações do peso espacial.
  grade$ocorrencias <- round(rowSums(grade[cols_num]))
  grade <- filter(grade, ocorrencias > 0)

  grade_sf <- cell_to_polygon(grade$h3_address, simple = FALSE) |>
    left_join(grade, by = "h3_address")

  # escala Jenks (quebras naturais) — com poucos hexágonos distintos, Jenks
  # pode falhar; cai para quantil nesse caso, sem travar a função.
  n_classes_efetivo <- min(n_classes, length(unique(grade_sf$ocorrencias)))
  quebras <- tryCatch(
    classIntervals(grade_sf$ocorrencias, n = n_classes_efetivo, style = "jenks")$brks,
    error = function(e) classIntervals(grade_sf$ocorrencias, n = n_classes_efetivo, style = "quantile")$brks
  )
  quebras <- sort(unique(quebras))

  grade_sf$faixa <- cut(
    grade_sf$ocorrencias,
    breaks = quebras,
    include.lowest = TRUE,
    dig.lab = 5
  )

  paleta <- colorRampPalette(c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b"))(nlevels(grade_sf$faixa))

  titulo_local <- if (!is.null(municipio)) municipio_resolvido else "Estado de São Paulo"

  mapa <- ggplot(grade_sf) +
    geom_sf(aes(fill = faixa), color = "white", linewidth = 0.05)

  if (!is.null(poligono_bairros)) {
    mapa <- mapa +
      geom_sf(data = poligono_bairros, fill = NA, color = "grey50", linewidth = 0.15)
  }

  if (!is.null(poligono_municipio)) {
    mapa <- mapa +
      geom_sf(data = poligono_municipio, fill = NA, color = "grey30", linewidth = 0.4) +
      coord_sf(xlim = st_bbox(poligono_municipio)[c("xmin", "xmax")],
               ylim = st_bbox(poligono_municipio)[c("ymin", "ymax")])
  }

  mapa +
    scale_fill_manual(values = paleta, name = "Ocorrências\n(Jenks)", drop = FALSE) +
    labs(
      title    = crime_resolvido,
      subtitle = paste0(titulo_local, " — células H3 resolução ", resolucao),
      caption  = "Fonte: SSP-SP — SPDadosCriminais_jan_ago2026 (geolocalização tratada, ver README.md)"
    ) +
    theme_void() +
    theme(
      plot.title      = element_text(face = "bold"),
      legend.position = "right"
    )
}
