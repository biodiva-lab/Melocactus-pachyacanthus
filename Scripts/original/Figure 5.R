
dif<-read.table("clipboard", h=T,encoding = "latin-1", sep="\t")
##Area-dif com ecoregioes##

unique(dif$eco)
dif$eco<-gsub("Complexo da Chapada Diamantina", "Chapada Diamantina Complex", dif$eco)
dif$eco<-gsub("Dunas do SÃ£o Francisco", "São Francisco Dunes", dif$eco)
dif$eco<-gsub("DepressÃ£o Sertaneja Meridional", "Southern Sertaneja Depression", dif$eco)


library(dplyr)
library(ggplot2)

# ------- ORGANIZAR DADO -------
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

# ------- PLOT PRESENTE -------
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

# ------- COMBINAR -------
rp_eco_km2 <- p_eco_km2 + theme(legend.position = "top")
p_eco_pres <- p_eco_pres + theme(legend.position = "none")
p_eco_km2 / p_eco_pres 
png("fig5_pct.png", height = 22.5, width=27, unit="cm",res=600)
dev.off()



