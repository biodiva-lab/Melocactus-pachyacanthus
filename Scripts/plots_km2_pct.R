library(ggplot2)
library(patchwork)
library(dplyr)
library(stringr)
dir()

val<-read.table("area_sens_test.csv",h=T,encoding = "latin1", sep=",")
unique(val$eco)
val$eco<-gsub("Ã£","ã",val$eco)

unique(val$metric)
val<-val[val$metric=="Max Sorensen",]
val$metric<-NULL
# ------- organize data-------
df_areas_eco <- val %>%
  rename(ecoregion = eco, area_km2 = value) %>%
  mutate(
    tipo = case_when(
      str_detect(name, "current") ~ "Current",
      str_detect(name, "fire")    ~ "CF",
      str_detect(name, "lulc")    ~ "LULC",
      str_detect(name, "combined") ~ "Combined",
      str_detect(name, "SSP245")  ~ "SSP2-4.5",
      str_detect(name, "SSP585")  ~ "SSP5-8.5"
    ),
    ano = case_when(
      str_detect(name, "current|SSP") ~ NA_character_,
      str_detect(name, "1995") ~ "1995",
      str_detect(name, "2005") ~ "2005",
      str_detect(name, "2015") ~ "2015",
      str_detect(name, "2022") ~ "2022"
    ),
    ano = case_when(
      tipo == "Current"  ~ "Current",
      tipo %in% c("SSP2-4.5", "SSP5-8.5") ~ "2041-2060",
      TRUE ~ ano
    )
  ) %>%
  select(ecoregion, tipo, ano, area_km2)

# ------- add total -------
df_areas_eco <- bind_rows(
  df_areas_eco,
  df_areas_eco %>%
    group_by(tipo, ano) %>%
    summarise(area_km2 = round(sum(area_km2), 2), .groups = "drop") %>%
    mutate(ecoregion = "Total")
)

# ------- reference area per ecoregion -------
area_ref_eco <- df_areas_eco %>%
  filter(tipo == "Current") %>%
  select(ecoregion, area_km2) %>%
  rename(area_ref = area_km2)

# ------- add PCT -------
df_areas_eco <- df_areas_eco %>%
  left_join(area_ref_eco, by = "ecoregion") %>%
  mutate(
    pct       = round(((area_km2 - area_ref) / area_ref) * 100, 2),
    ano       = factor(ano, levels = c("Current", "1995", "2005", "2015", "2022", "2041-2060")),
    tipo      = factor(tipo, levels = c("Current", "CF", "LULC", "Combined", "SSP2-4.5", "SSP5-8.5")),
    ecoregion = factor(ecoregion, levels = c(unique(val$eco), "Total"))
  ) %>%
  select(-area_ref)

# ------- df_areas -------
area_total <- df_areas_eco %>%
  filter(tipo == "Current", ecoregion == "Total") %>%
  pull(area_km2)

df_areas <- df_areas_eco %>%
  filter(ecoregion == "Total", tipo != "Current") %>%
  mutate(
    ano  = factor(ano,  levels = c("1995", "2005", "2015", "2022", "2041-2060")),
    tipo = factor(tipo, levels = c("CF", "LULC", "Combined", "SSP2-4.5", "SSP5-8.5"))
  )

area_label <- "Current suitable area (133,327.5 km²)"

bar_width <- 0.6

# ------- PLOT PRESENT KM2 -------
p_present_km2 <- ggplot(
  df_areas %>% filter(!tipo %in% c("SSP2-4.5", "SSP5-8.5")),
  aes(x = ano, y = area_km2, fill = tipo)
) +
  geom_bar(stat = "identity", position = position_dodge(bar_width), width = bar_width) +
  geom_hline(yintercept = area_total, linetype = "dashed", color = "black") +
  annotate("text", x = 0.5, y = area_total * 1.05,
           label = area_label, hjust = 0, size = 4) +
  scale_fill_manual(values = c("CF" = "#7A0403", "LULC" = "#DAE235", "Combined" = "#FB8322")) +
  scale_y_continuous(limits = c(0, area_total * 1.1),
                     expand = c(0, 0),
                     labels = scales::comma) +
  labs(x = NULL, y = expression("Suitable area (km"^2*")"), fill = NULL) +
  theme_bw() +
  theme(
    legend.position = "top",
    axis.text.x     = element_blank(),
    axis.ticks.x    = element_blank()
  )

# ------- PLOT PRESENT PCT -------
p_present_pct <- ggplot(
  df_areas %>% filter(!tipo %in% c("SSP2-4.5", "SSP5-8.5")),
  aes(x = ano, y = pct, fill = tipo)
) +
  geom_bar(stat = "identity", position = position_dodge(bar_width), width = bar_width) +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    position = position_dodge(bar_width),
    vjust = 1.5,
    size = 3)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("CF" = "#7A0403", "LULC" = "#DAE235", "Combined" = "#FB8322")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,-80)) +
  labs(x = "Year", y = "Change in suitable area (%)", fill = NULL) +
  theme_bw() +
  theme(legend.position = "none")

# ------- PLOT FUTURE KM2 -------
p_future_km2 <- ggplot(
  df_areas %>% filter(tipo %in% c("SSP2-4.5", "SSP5-8.5")),
  aes(x = ano, y = area_km2, fill = tipo)
) +
  geom_bar(stat = "identity", position = position_dodge(bar_width), width = bar_width) +
  geom_hline(yintercept = area_total, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("SSP2-4.5" = "#1BCFD5", "SSP5-8.5" = "#30123B")) +
  scale_y_continuous(limits = c(0, area_total * 1.1),
                     expand = c(0, 0),
                      labels = scales::comma) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_bw() +
  theme(
    legend.position = "top",
    axis.text.x     = element_blank(),
    axis.ticks.x    = element_blank(),
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_blank()
  )

# ------- PLOT FUTURE PCT -------
p_future_pct <- ggplot(
  df_areas %>% filter(tipo %in% c("SSP2-4.5", "SSP5-8.5")),
  aes(x = ano, y = pct, fill = tipo)
) +
  geom_bar(stat = "identity", position = position_dodge(bar_width), width = bar_width) +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    position = position_dodge(bar_width),
    vjust = 1.5,
    size = 3)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("SSP2-4.5" = "#1BCFD5", "SSP5-8.5" = "#30123B")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,-80)) +
  labs(x = "Year", y = NULL, fill = NULL) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_blank()
  )

# ------- combine -------
(p_present_km2 + p_future_km2 + plot_layout(widths = c(4, 1), guides = "keep")) /
  (p_present_pct + p_future_pct + plot_layout(widths = c(4, 1), guides = "keep"))

png("fig4_pct.png", height = 22.5, width=27, unit="cm",res=600)
dev.off()
0.9*25
