library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(ggspatial)

# ------- DADOS -------
brazil      <- ne_states(country = "Brazil", returnclass = "sf")
south_am    <- ne_countries(continent = "South America", returnclass = "sf")
sp_region_sf<-read_sf(choose.files())
sp_region_sf <- st_set_crs(sp_region_sf, 4326)

sp_region_sf <- sp_region_sf %>%
  mutate(NOMPAISA = case_when(
    grepl("Chapad",  NOMPAISA) ~ "Chapada Diamantina Complex",
    grepl("Duna",    NOMPAISA) ~ "São Francisco Dunes",
    grepl("Depress", NOMPAISA) ~ "Southern Sertaneja Depression"
  ))

eco_colors <- c(
  "Chapada Diamantina Complex" = "#f7b183",
  "São Francisco Dunes"         = "#aa4c3a",
  "Southern Sertaneja Depression"            = "#edefa2"
)

# Centroides dos estados para abreviações
brazil_centroids <- brazil %>%
  st_make_valid() %>%
  st_centroid()



# ------- MAPA PRINCIPAL -------
(p_map <- ggplot() +
  geom_sf(data = brazil, fill = "gray85", color = "gray50", linewidth = 0.3) +
  geom_sf(data = sp_region_sf, aes(fill = NOMPAISA), color = "black",
          linewidth = 0.5, alpha = 0.8)+
  geom_point(
    data = melocactus,
    aes(x = decimalLongitude, y = decimalLatitude, shape = "Occurrence"),
    color = "black", size = 1.5, alpha = 0.7
  ) +
  scale_shape_manual(values = c("Occurrence" = 16), name = NULL) +
   annotation_scale(location = "br", width_hint = 0.3) +
   annotation_north_arrow(
     location = "tr",
     pad_y = unit(0.5, "cm"),
     style = north_arrow_fancy_orienteering()
   ) +
  scale_fill_manual(values = eco_colors, name = "Ecoregion") +
  coord_sf(xlim = c(-47, -34), ylim = c(-18, -5)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#AED6F1"),
    legend.position  = "none",
    panel.grid       = element_blank(),
    axis.text        = element_text(size = 8)
  )+
   guides(
     fill  = guide_legend(title = "Ecoregion", order = 1),
     shape = guide_legend(title = NULL, order = 2)
   ))

# ------- INSET -------
(p_inset <- ggplot() +
  geom_sf(data = south_am, fill = "gray92", color = "gray92", linewidth = 0.2) +
  geom_sf(data = brazil, fill = "gray75", color = "gray50", linewidth = 0.2) +
  geom_sf(data = sp_region_sf, fill = "gray50", color = "gray30", linewidth = 0.05) +
  coord_sf(xlim = c(-74, -34), ylim = c(-34, 6)) +
  theme_void() +
   theme(
     panel.background  = element_rect(fill = "#AED6F1", color = "black", linewidth = 0.8),
     panel.border      = element_rect(fill = NA, color = "black", linewidth = 0.8),
     plot.margin       = margin(5, 0, 0, 15)
   )
 )

# ------- LEGENDA ISOLADA -------
(p_legend <- ggplot() +
  geom_sf(data = sp_region_sf, aes(fill = NOMPAISA), color = "black",
          linewidth = 0.5, alpha = 0.8) +
  geom_point(
    data = melocactus,
    aes(x = decimalLongitude, y = decimalLatitude, shape = "Occurrence"),
    color = "black", size = 1.5, alpha = 0.7
  ) +
  scale_fill_manual(values = eco_colors, name = "Ecoregion") +
  scale_shape_manual(values = c("Occurrence" = 16), name = NULL) +
  theme_void() +
  theme(
    legend.position = "left",
    legend.title    = element_text(face = "bold", size = 10),
    legend.text     = element_text(size = 9)
  ) +
  guides(
    fill  = guide_legend(title = "Ecoregion", order = 1),
    shape = guide_legend(title = NULL, order = 2)
  ))

# Extrair só a legenda
library(cowplot)
legend_only <- get_legend(p_legend)

# ------- COMBINAR -------

col_esquerda <- plot_grid(
  p_inset,
  legend_only,
  ncol = 1,
  rel_heights = c(2, 1)
)

plot_grid(
  col_esquerda, p_map,
  nrow = 1,
  rel_widths = c(1, 3)
)

ggsave("mapa_localizacao.pdf", width = 12, height = 8, 
       device = cairo_pdf)
