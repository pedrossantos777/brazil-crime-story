# ============================================================================
# Mapa coroplético hexagonal (H3) sobre camada base do OpenStreetMap
# ============================================================================
# Pré-requisito: grade_hex_r8 e grade_hex_r9 já existem em memória (gerados
# em mapa_choropletico_sp.R) — sf de polígonos hexagonais H3 (resoluções 8
# e 9) com a coluna `ocorrencias`.
#
# Abordagem ESTÁTICA (sem dependência de um widget HTML/JS como o mapgl):
#   1) maptiles::get_tiles() baixa os tiles raster do OpenStreetMap que
#      cobrem a extensão da grade hexagonal e os recorta (crop = TRUE);
#   2) tidyterra::geom_spatraster_rgb() desenha esse raster como camada
#      base dentro do próprio ggplot2;
#   3) os hexágonos são sobrepostos com geom_sf(), semitransparentes, para
#      que as ruas/bairros do OSM continuem visíveis por baixo.
#
# A alternativa interativa (mapgl, que usa MapLibre GL JS) está comentada
# ao final do script, caso um mapa navegável seja necessário depois.
# ============================================================================

library(sf)
library(ggplot2)
library(maptiles)
library(tidyterra)
library(classInt)

paleta_hex <- c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b")

#' Gera rótulos de faixa ("< b1", "b1 - b2", ..., "bk+") a partir de um
#' vetor de quebras de classIntervals() (length = n classes + 1).
rotular_quebras <- function(brks) {
  k <- length(brks) - 1
  rotulos <- character(k)
  rotulos[1] <- paste0("< ", brks[2])
  if (k > 2) {
    for (i in 2:(k - 1)) rotulos[i] <- paste0(brks[i], " - ", brks[i + 1])
  }
  rotulos[k] <- paste0(brks[k], "+")
  rotulos
}

# 1. Extensão (bbox) comum às duas grades, com uma margem pequena para o
#    basemap não cortar nenhum hexágono na borda.
bbox_grade <- st_bbox(grade_hex_r8)
margem <- 0.01 # graus (~1 km)
bbox_tiles <- st_as_sfc(st_bbox(c(
  xmin = unname(bbox_grade["xmin"]) - margem, xmax = unname(bbox_grade["xmax"]) + margem,
  ymin = unname(bbox_grade["ymin"]) - margem, ymax = unname(bbox_grade["ymax"]) + margem
), crs = st_crs(grade_hex_r8)))

# 2. Baixa (e cacheia localmente) os tiles do OpenStreetMap cobrindo SP.
#    zoom 12 dá um bom equilíbrio entre nitidez das ruas e tempo/tamanho
#    de download para a extensão inteira do município.
osm_tiles <- get_tiles(bbox_tiles, provider = "OpenStreetMap", zoom = 12, crop = TRUE)

#' Plota a grade hexagonal H3 sobreposta ao basemap do OpenStreetMap.
#'
#' A classificação usa quebras naturais (Jenks, via classInt) em vez de uma
#' escala contínua: a contagem de ocorrências é muito concentrada em poucos
#' hexágonos centrais, então uma escala linear deixava quase toda a grade na
#' cor mais clara. Jenks agrupa os valores em classes que minimizam a
#' variância dentro de cada uma, revelando melhor o gradiente nas periferias.
#'
#' @param grade_hex sf de hexágonos H3 com a coluna `ocorrencias`.
#' @param res resolução H3 usada (só para o subtítulo).
#' @param n_classes número de classes Jenks.
plotar_mapa_hex_osm <- function(grade_hex, res, n_classes = 5) {
  quebras <- classIntervals(grade_hex$ocorrencias, n = n_classes, style = "jenks")$brks
  rotulos <- rotular_quebras(quebras)
  grade_hex$classe <- cut(grade_hex$ocorrencias, breaks = quebras,
                           labels = rotulos, include.lowest = TRUE)

  ggplot() +
    geom_spatraster_rgb(data = osm_tiles, maxcell = 2e6) +
    geom_sf(
      data = grade_hex, aes(fill = classe),
      color = "white", linewidth = 0.05, alpha = 0.75
    ) +
    scale_fill_manual(
      values = setNames(paleta_hex, rotulos),
      name   = "Celulares\nroubados",
      drop   = FALSE
    ) +
    coord_sf(crs = st_crs(grade_hex), expand = FALSE) +
    labs(
      title    = "Furto/roubo de celular — Cidade de São Paulo",
      subtitle = paste0("Ocorrências por célula hexagonal H3 (quebras naturais, Jenks) — resolução ", res),
      caption  = "Fonte: SSP-SP — CelularesSubtraidos_2026 | Mapa: © OpenStreetMap contributors"
    ) +
    theme_void() +
    theme(
      plot.title      = element_text(face = "bold"),
      legend.position = "right"
    )
}

mapa_osm_res8 <- plotar_mapa_hex_osm(grade_hex_r8, 8)
mapa_osm_res9 <- plotar_mapa_hex_osm(grade_hex_r9, 9)

mapa_osm_res8
ggsave("sao_paulo_res8_celulares_osm.png", plot = mapa_osm_res8, width = 8, height = 8, dpi = 150)

mapa_osm_res9
ggsave("sao_paulo_res9_celulares_osm.png", plot = mapa_osm_res9, width = 8, height = 8, dpi = 150)

# ----------------------------------------------------------------------------
# Alternativa interativa (não estática) com mapgl (MapLibre GL JS):
# mesmos hexágonos, mas navegável (zoom/pan) e com legenda + barra de escala
# de distância nativas da biblioteca.
# ----------------------------------------------------------------------------

library(mapgl)

#' Monta o mapa interativo MapLibre com a grade hexagonal, título/legenda
#' informando o tema (furto/roubo de celular em SP) e a escala de cores.
#'
#' A classificação usa quebras naturais (Jenks) em vez de uma escala linear
#' contínua: como a contagem de ocorrências é muito concentrada em poucos
#' hexágonos centrais, uma escala linear deixava quase toda a grade na cor
#' mais clara. Jenks agrupa os valores em 5 classes que minimizam a variância
#' dentro de cada classe, revelando melhor o gradiente nas áreas periféricas.
#'
#' @param grade_hex sf de hexágonos H3 com a coluna `ocorrencias`.
#' @param res resolução H3 usada (para o título e o id da camada).
construir_mapa_mgl <- function(grade_hex, res) {
  classes <- step_jenks(data = grade_hex, column = "ocorrencias", n = 5, colors = paleta_hex)

  maplibre(style = carto_style("positron")) |>
    fit_bounds(grade_hex, animate = FALSE) |>
    add_fill_layer(
      id = paste0("hex_res", res), source = grade_hex,
      fill_color = classes$expression,
      fill_opacity = 0.75,
      fill_outline_color = "#ffffff",
      tooltip = "ocorrencias"
    ) |>
    add_legend(
      legend_title = htmltools::HTML(paste0(
        "<strong>Furto/roubo de celular</strong><br>Cidade de São Paulo - 2026 <br>",
        "<span style='font-weight:normal;font-size:11px'>",
        "Hexágono H3, quebras naturais (Jenks) — res. ", res, "</span>"
      )),
      values   = classes$labels,
      colors   = classes$colors,
      type     = "categorical",
      position = "top-left"
    ) |>
    add_scale_control(position = "bottom-left", unit = "metric")
}

mapa_mgl_res8 <- construir_mapa_mgl(grade_hex_r8, 8)
mapa_mgl_res9 <- construir_mapa_mgl(grade_hex_r9, 9)

mapa_mgl_res8
mapa_mgl_res9

# Exporta como HTML autocontido (abre em qualquer navegador, sem precisar
# do R rodando) para facilitar o compartilhamento dos mapas navegáveis.
htmlwidgets::saveWidget(mapa_mgl_res8, "sao_paulo_res8_celulares_mapgl.html", selfcontained = TRUE)
htmlwidgets::saveWidget(mapa_mgl_res9, "sao_paulo_res9_celulares_mapgl.html", selfcontained = TRUE)
# ----------------------------------------------------------------------------
