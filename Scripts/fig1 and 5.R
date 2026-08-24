library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(ggspatial)

# ------- Data -------
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

library(cowplot)
legend_only <- get_legend(p_legend)


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

ggsave("map_local.pdf", width = 12, height = 8, 
       device = cairo_pdf)

# Figure 5

dif<-read.table("clipboard", h=T,encoding = "latin-1", sep="\t")

## difference area##

unique(dif$eco)
dif$eco<-gsub("Complexo da Chapada Diamantina", "Chapada Diamantina Complex", dif$eco)
dif$eco<-gsub("Dunas do SÃ£o Francisco", "São Francisco Dunes", dif$eco)
dif$eco<-gsub("DepressÃ£o Sertaneja Meridional", "Southern Sertaneja Depression", dif$eco)


library(dplyr)
library(ggplot2)

# ------- data -------
df_dif <- dif %>%
  filter(cenario != "current") %>%
  mutate(
    tipo = case_when(
      str_detect(cenario, "fire")     ~ "CF",
      str_detect(cenario, "lulc")     ~ "LULC",
      str_detect(cenario, "combined") ~ "Combined",
      str_detect(cenario, "SSP245")   ~ "SSP2-4.5",
      str_detect(cenario, "SSP585")   ~ "SSP5-8.5"
    ),
    ano = case_when(
      str_detect(cenario, "1995") ~ "1995",
      str_detect(cenario, "2005") ~ "2005",
      str_detect(cenario, "2015") ~ "2015",
      str_detect(cenario, "2022") ~ "2022",
      tipo %in% c("SSP2-4.5", "SSP5-8.5") ~ "2041-2060"
    ),
    tipo = factor(tipo, levels = c("CF", "LULC", "Combined", "SSP2-4.5", "SSP5-8.5")),
    ano  = factor(ano,  levels = c("1995", "2005", "2015", "2022", "2041-2060")),
    eco  = factor(eco,  levels = unique(dif$eco))
  )

# ------- PLOT PRESENT -------
(p_eco_pres <- ggplot(
  df_dif %>% filter(!tipo %in% c("SSP2-4.5", "SSP5-8.5")),
  aes(x = ano, y = diferenca_pct, fill = tipo)
) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(
    aes(label = paste0(round(diferenca_pct, 1))),
    position = position_dodge(0.9),
    vjust = 1.5,
    size = 2.5
  )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("CF" = "#7A0403", "LULC" = "#DAE235", "Combined" = "#FB8322")) +
  scale_y_continuous(expand = c(0.05, 0)) +
  labs(x = NULL, y = "Change in suitable area (%)", fill = NULL) +
  facet_wrap(~ eco, nrow = 1) +
  theme_bw() +
  theme(
    legend.position = "",
    axis.text.x     = element_text(size = 8),
    strip.text      = element_text(face = "bold", size = 9)
  ))

# ------- PLOT KM2 -------
p_eco_km2 <- ggplot(
  df_dif %>% filter(!tipo %in% c("SSP2-4.5", "SSP5-8.5")),
  aes(x = ano, y = area_km2, fill = tipo)
) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("CF" = "#7A0403", "LULC" = "#DAE235", "Combined" = "#FB8322")) +
  scale_y_continuous(expand = c(0.05, 0), labels = scales::label_number(big.mark = ",")) +
  labs(x = NULL, y = expression("Suitable area (km"^2*")"), fill = NULL) +
  facet_wrap(~ eco, nrow = 1) +
  theme_bw() +
  theme(
    legend.position = "top",
    axis.text.x     = element_blank(),
    axis.ticks.x    = element_blank(),
    strip.text      = element_text(face = "bold", size = 9)
  )

# ------- combine -------
rp_eco_km2 <- p_eco_km2 + theme(legend.position = "top")
p_eco_pres <- p_eco_pres + theme(legend.position = "none")
p_eco_km2 / p_eco_pres 
png("fig5_pct.png", height = 22.5, width=27, unit="cm",res=600)
dev.off()



