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

# H3 trabalha em coordenadas geográficas (graus); garante o CRS correto.
pontos_sp <- st_transform(df_cel_sp, crs = 4326)

# Descarta coordenadas corrompidas/fora do município (ex.: valores como
# "-4664112" resultantes de vírgulas mal tratadas na base de origem) —
# sem isso, esses pontos gerariam hexágonos muito distantes de SP e
# distorceriam a extensão (bounding box) do mapa.
# Filtra por subset de linhas (sem operação geométrica, ex. st_crop/
# st_intersection) para garantir que a geometria continue sfc_POINT puro —
# algumas versões de sf/GEOS fazem st_crop devolver sfc_GEOMETRY genérico,
# o que quebra a checagem estrita do h3jsr (point_to_cell exige sfc_POINT).
coords_sp <- st_coordinates(pontos_sp)
dentro_sp <- coords_sp[, "X"] >= -46.83 & coords_sp[, "X"] <= -46.35 &
  coords_sp[, "Y"] >= -24.02 & coords_sp[, "Y"] <= -23.35
pontos_sp <- pontos_sp[dentro_sp, ]

# Checagem de sanidade: se isso imprimir um número muito baixo (ex. "2"),
# o objeto `df_cel_sp`/`pontos_sp` está desatualizado na sessão — rode
# novamente exploratory_sp_celulares.R (de preferência numa sessão limpa)
# antes deste script.
message("Pontos válidos dentro do município de SP: ", nrow(pontos_sp))
stopifnot(
  "pontos_sp tem poucas observações — refaça exploratory_sp_celulares.R" =
    nrow(pontos_sp) > 1000
)

#' Gera a grade hexagonal H3 agregada para uma resolução dada.
#'
#' @param pontos sf de pontos em WGS84 (EPSG:4326).
#' @param res resolução H3 (0-15).
#' @return sf de polígonos hexagonais com a coluna `ocorrencias`.
gerar_grade_hex <- function(pontos, res) {
  h3_index <- point_to_cell(pontos, res = res, simple = TRUE)

  contagem <- tibble(h3_address = h3_index) |>
    count(h3_address, name = "ocorrencias")

  cell_to_polygon(contagem$h3_address, simple = FALSE) |>
    left_join(contagem, by = "h3_address")
}

grade_hex_r8 <- gerar_grade_hex(pontos_sp, res = 8)
grade_hex_r9 <- gerar_grade_hex(pontos_sp, res = 9)

#' Plota o coroplético hexagonal para uma grade/resolução já agregada.
plotar_mapa_hex <- function(grade_hex, res) {
  ggplot(grade_hex) +
    geom_sf(aes(fill = ocorrencias), color = "white", linewidth = 0.05) +
    scale_fill_gradientn(
      colors = c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b"),
      name   = "Celulares\nroubados"
    ) +
    labs(
      title    = "Furto/roubo de celular — Cidade de São Paulo",
      subtitle = paste0("Ocorrências por célula hexagonal H3 — resolução ", res),
      caption  = "Fonte: SSP-SP — CelularesSubtraidos_2026"
    ) +
    theme_void() +
    theme(
      plot.title      = element_text(face = "bold"),
      legend.position = "right"
    )
}

mapa_res8 <- plotar_mapa_hex(grade_hex_r8, 8)
mapa_res9 <- plotar_mapa_hex(grade_hex_r9, 9)

mapa_res8
ggsave("sao_paulo_res8_celulares.png", plot = mapa_res8)

mapa_res9
ggsave("sao_paulo_res9_celulares.png", plot = mapa_res9)
