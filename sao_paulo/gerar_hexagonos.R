# ============================================================================
# Mapa coroplético — incidência de furto/roubo de celular na cidade de SP
# ============================================================================
# Pré-requisito: df_cel_sp já existe em memória (gerado em
# exploratory_sp_celulares.R) e é um objeto `sf` de PONTOS em WGS84
# (EPSG:4326) — uma linha por boletim de ocorrência.
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
# O processo é repetido nas resoluções H3 8 (~0,74 km² por hexágono) e
# 9 (~0,11 km²), e ambos os mapas são plotados.
# ============================================================================

library(sf)
library(dplyr)
library(ggplot2)
library(h3jsr)
library(arrow)

# H3 trabalha em coordenadas geográficas (graus); garante o CRS correto.
pontos_sp <- st_transform(df_cel_sp, crs = 4326)

# Em vez de descartar coordenadas corrompidas/fora do município (ex.:
# valores como "-4664112" resultantes de vírgulas mal tratadas na base de
# origem), marca cada linha com uma coluna booleana `coordenada_valida`.
# Isso preserva o dataframe inteiro (nenhum boletim é perdido) e evita que
# esses pontos distorçam a extensão (bounding box) do mapa, já que o H3 só
# será calculado para as linhas marcadas como válidas.
coords_sp <- st_coordinates(pontos_sp)
pontos_sp$coordenada_valida <- coords_sp[, "X"] >= -46.83 & coords_sp[, "X"] <= -46.35 &
  coords_sp[, "Y"] >= -24.02 & coords_sp[, "Y"] <= -23.35

# Checagem de sanidade: se isso imprimir um número muito baixo (ex. "2"),
# o objeto `df_cel_sp`/`pontos_sp` está desatualizado na sessão — rode
# novamente exploratory_sp_celulares.R (de preferência numa sessão limpa)
# antes deste script.
message(
  "Pontos válidos dentro do município de SP: ",
  sum(pontos_sp$coordenada_valida), " de ", nrow(pontos_sp), " totais"
)
stopifnot(
  "pontos_sp tem poucas observações válidas — refaça exploratory_sp_celulares.R" =
    sum(pontos_sp$coordenada_valida) > 1000
)

# Indexa cada boletim de ocorrência na célula H3 que o contém, nas
# resoluções 8 (~0,74 km²) e 9 (~0,11 km²), e grava o índice como coluna
# no próprio dataframe — assim cada linha de df_cel_sp fica marcada com o
# hexágono a que pertence em cada resolução. point_to_cell só é chamado
# sobre as linhas válidas (subset por linha, não por geometria — mantém
# sfc_POINT puro, que é o que point_to_cell exige); as inválidas ficam
# com NA nas colunas de hexágono.
pontos_sp$h3_res8 <- NA_character_
pontos_sp$h3_res9 <- NA_character_

pontos_sp$h3_res8[pontos_sp$coordenada_valida] <- point_to_cell(
  pontos_sp[pontos_sp$coordenada_valida, ], res = 8, simple = TRUE
)
pontos_sp$h3_res9[pontos_sp$coordenada_valida] <- point_to_cell(
  pontos_sp[pontos_sp$coordenada_valida, ], res = 9, simple = TRUE
)

# Propaga as colunas de hexágono (e a flag de validade) para df_cel_sp,
# agora com todas as linhas originais preservadas.
df_cel_sp <- pontos_sp

#' Gera a grade hexagonal H3 agregada para uma resolução dada.
#'
#' @param pontos sf de pontos em WGS84 (EPSG:4326), já com a coluna de
#'   índice H3 correspondente (`h3_res8` ou `h3_res9`). Linhas com
#'   coordenada inválida (NA na coluna de hexágono) são ignoradas.
#' @param col_h3 nome da coluna com o índice H3 pré-calculado.
#' @return sf de polígonos hexagonais com a coluna `ocorrencias`.
gerar_grade_hex <- function(pontos, col_h3) {
  contagem <- tibble(h3_address = pontos[[col_h3]]) |>
    filter(!is.na(h3_address)) |>
    count(h3_address, name = "ocorrencias")

  cell_to_polygon(contagem$h3_address, simple = FALSE) |>
    left_join(contagem, by = "h3_address")
}

grade_hex_r8 <- gerar_grade_hex(pontos_sp, col_h3 = "h3_res8")
grade_hex_r9 <- gerar_grade_hex(pontos_sp, col_h3 = "h3_res9")

# Desagrega a coluna de geometria (POINT) em colunas explícitas de
# longitude/latitude e remove a geometria — parquet é um formato tabular
# e não tem suporte nativo para geometria sf.
coords_sp_final <- st_coordinates(df_cel_sp)
df_cel_sp$LONGITUDE <- coords_sp_final[, "X"]
df_cel_sp$LATITUDE  <- coords_sp_final[, "Y"]

df_cel_sp_tabular <- st_drop_geometry(df_cel_sp)

write_parquet(df_cel_sp_tabular, "dados/df_cel_sp_hex.parquet")
