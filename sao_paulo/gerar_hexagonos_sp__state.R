# ============================================================================
# Mapa coroplético — incidência de furto/roubo de celular no estado de SP
# ============================================================================
# Pré-requisito: df_cel já existe em memória (gerado em
# exploratory_sp_celulares.R) e é um objeto `sf` de PONTOS em WGS84
# (EPSG:4326) — uma linha por boletim de ocorrência. Diferente de
# df_cel_sp (restrito ao município da capital), df_cel cobre todos os
# municípios do estado presentes na base (a base de origem já é estadual,
# da SSP-SP).
#
# Como um coroplético pinta ÁREAS (não pontos), o processo abaixo usa o
# pacote h3jsr (bindings para a biblioteca H3 do Uber) para:
#   1) indexar cada ponto diretamente na célula hexagonal H3 que o contém
#      (point_to_cell — operação baseada em índice, não em geometria, logo
#      muito mais rápida que um grid manual + st_intersects);
#   2) agregar (contar) quantas ocorrências caem em cada célula;
#   3) reconstruir a geometria apenas das células únicas ocupadas
#      (cell_to_polygon) e pintar cada uma conforme a contagem.
#
# O processo é repetido nas resoluções H3 4 (~1 770 km² por hexágono),
# 6 (~36 km²), 8 (~0,74 km²) e 9 (~0,11 km²).
# ============================================================================

library(sf)
library(dplyr)
library(ggplot2)
library(h3jsr)
library(arrow)

# H3 trabalha em coordenadas geográficas (graus); garante o CRS correto.
pontos_estado <- st_transform(df_cel, crs = 4326)

# Em vez de descartar coordenadas corrompidas/fora do estado (ex.: valores
# como "-4664112" resultantes de vírgulas mal tratadas na base de origem),
# marca cada linha com uma coluna booleana `coordenada_valida`. Isso
# preserva o dataframe inteiro (nenhum boletim é perdido) e evita que
# esses pontos distorçam a extensão (bounding box) do mapa, já que o H3 só
# será calculado para as linhas marcadas como válidas. A caixa usada aqui
# é a do ESTADO de São Paulo (bem mais ampla que a da capital).
coords_estado <- st_coordinates(pontos_estado)
pontos_estado$coordenada_valida <-
  coords_estado[, "X"] >= -53.5 & coords_estado[, "X"] <= -44.0 &
  coords_estado[, "Y"] >= -25.5 & coords_estado[, "Y"] <= -19.5

# Checagem de sanidade: se isso imprimir um número muito baixo, o objeto
# `df_cel`/`pontos_estado` está desatualizado na sessão — rode novamente
# exploratory_sp_celulares.R (de preferência numa sessão limpa) antes
# deste script.
message(
  "Pontos válidos dentro do estado de SP: ",
  sum(pontos_estado$coordenada_valida), " de ", nrow(pontos_estado), " totais"
)
stopifnot(
  "pontos_estado tem poucas observações válidas — refaça exploratory_sp_celulares.R" =
    sum(pontos_estado$coordenada_valida) > 1000
)

# Resoluções H3 a gerar, da mais grossa (4) à mais fina (9).
resolucoes_h3 <- c(4, 6, 8, 9)

# Indexa cada boletim de ocorrência na célula H3 que o contém, em cada
# resolução, e grava o índice como coluna no próprio dataframe — assim
# cada linha fica marcada com o hexágono a que pertence em cada resolução
# (colunas h3_res4, h3_res6, h3_res8, h3_res9). point_to_cell só é chamado
# sobre as linhas válidas (subset por linha, não por geometria — mantém
# sfc_POINT puro, que é o que point_to_cell exige); as inválidas ficam
# com NA nas colunas de hexágono.
for (res in resolucoes_h3) {
  col <- paste0("h3_res", res)
  pontos_estado[[col]] <- NA_character_
  pontos_estado[[col]][pontos_estado$coordenada_valida] <- point_to_cell(
    pontos_estado[pontos_estado$coordenada_valida, ], res = res, simple = TRUE
  )
}

# Propaga as colunas de hexágono (e a flag de validade) para df_cel, agora
# com todas as linhas originais preservadas.
df_cel <- pontos_estado

#' Gera a grade hexagonal H3 agregada para uma resolução dada.
#'
#' @param pontos sf de pontos em WGS84 (EPSG:4326), já com a coluna de
#'   índice H3 correspondente (ex.: `h3_res8`). Linhas com coordenada
#'   inválida (NA na coluna de hexágono) são ignoradas.
#' @param col_h3 nome da coluna com o índice H3 pré-calculado.
#' @return sf de polígonos hexagonais com a coluna `ocorrencias`.
gerar_grade_hex <- function(pontos, col_h3) {
  contagem <- tibble(h3_address = pontos[[col_h3]]) |>
    filter(!is.na(h3_address)) |>
    count(h3_address, name = "ocorrencias")

  cell_to_polygon(contagem$h3_address, simple = FALSE) |>
    left_join(contagem, by = "h3_address")
}

# Gera uma grade agregada por resolução: grades_hex[["r4"]], ["r6"],
# ["r8"], ["r9"].
grades_hex <- setNames(
  lapply(resolucoes_h3, function(res) {
    gerar_grade_hex(pontos_estado, col_h3 = paste0("h3_res", res))
  }),
  paste0("r", resolucoes_h3)
)

grade_hex_r4 <- grades_hex[["r4"]]
grade_hex_r6 <- grades_hex[["r6"]]
grade_hex_r8 <- grades_hex[["r8"]]
grade_hex_r9 <- grades_hex[["r9"]]

# Desagrega a coluna de geometria (POINT) em colunas explícitas de
# longitude/latitude e remove a geometria — parquet é um formato tabular
# e não tem suporte nativo para geometria sf.
coords_estado_final <- st_coordinates(df_cel)
df_cel$LONGITUDE <- coords_estado_final[, "X"]
df_cel$LATITUDE  <- coords_estado_final[, "Y"]

df_cel_tabular <- st_drop_geometry(df_cel)

write_parquet(df_cel_tabular, "dados/df_cel_estado_hex.parquet")

