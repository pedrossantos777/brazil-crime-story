
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
