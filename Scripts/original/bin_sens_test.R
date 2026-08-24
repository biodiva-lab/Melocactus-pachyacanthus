
thresholds
thr<-thresholds[4,2]
thr_name<-thresholds[4,3]

##FOGO
f_bin<-f
plot(f_bin)

f_bin[f_bin<thr]<-0
f_bin[f_bin>=thr]<-1
plot(f_bin)

for(i in 2:5){
  writeRaster(f_bin[[i]], filename = paste(thr_name,"_CF_",names(f_bin[[i]]),".tif", sep=""))
}

valores2<-exact_extract(f_bin,sp_region_sf,
                        coverage_area=T,
                        fun="sum")
valores2<-valores2/1e+6
valores2$ecoregiao<-sp_region_sf$NOMPAISA
names(valores2)<-gsub("sum.","",names(valores2))

##LULC
lulc_bin<-lulc_pres
plot(lulc_bin)

lulc_bin[lulc_bin<thr]<-0
lulc_bin[lulc_bin>=thr]<-1
plot(lulc_bin)

for(i in 1:4){
  writeRaster(lulc_bin[[i]], filename = paste(thr_name,"_LULC_",names(lulc_bin[[i]]),".tif", sep=""))
}

valores3<-exact_extract(lulc_bin,sp_region_sf,
                        coverage_area=T,
                        fun="sum")
valores3<-valores3/1e+6
valores3$ecoregiao<-sp_region_sf$NOMPAISA
names(valores3)<-gsub("sum.","",names(valores3))

##Combined
combined_bin<-combined_pres
plot(combined_bin)

combined_bin[combined_bin<thr]<-0
combined_bin[combined_bin>=thr]<-1
plot(combined_bin)

for(i in 1:4){
  writeRaster(combined_bin[[i]], filename = paste(thr_name,"_Combined_",names(combined_bin[[i]]),".tif", sep=""))
}

valores4<-exact_extract(combined_bin,sp_region_sf,
                        coverage_area=T,
                        fun="sum")
valores4<-valores4/1e+6
names(valores4)<-gsub("sum.","",names(valores4))

##CRIANDO TABELA#####
names(valores2)
names(valores3)
names(valores4)

names(valores2)[2:5]<-paste("fire_",names(valores2)[2:5])
valores3$ecoregiao<-NULL
names(valores3)<-paste("lulc_",names(valores3))
names(valores4)<-paste("combined",names(valores4))

data<-cbind(valores2,valores3,valores4)
data$ecoregiao<-NULL
data$eco<-sp_region_sf$NOMPAISA
val<-pivot_longer(data, 1:15) 

val$metric<-thr_name

val<-as.data.frame(val)

val2<-rbind(val2,val)


write.csv(val2, "area_sens_test.csv",row.names = F)


# Organizar val2 da mesma forma que val
df_sens <- val2 %>%
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
      tipo == "Current"  ~ "Current",
      tipo %in% c("SSP2-4.5", "SSP5-8.5") ~ "2041-2060",
      str_detect(name, "1995") ~ "1995",
      str_detect(name, "2005") ~ "2005",
      str_detect(name, "2015") ~ "2015",
      str_detect(name, "2022") ~ "2022"
    ),
    ano  = factor(ano,  levels = c("Current", "1995", "2005", "2015", "2022", "2041-2060")),
    tipo = factor(tipo, levels = c("Current", "CF", "LULC", "Combined", "SSP2-4.5", "SSP5-8.5"))
  )

# Adicionar total por metric
df_sens <- bind_rows(
  df_sens,
  df_sens %>%
    group_by(tipo, ano, metric) %>%
    summarise(area_km2 = round(sum(area_km2), 2), .groups = "drop") %>%
    mutate(ecoregion = "Total")
)

# Extrair área current por metric
df_current_sens <- df_sens %>%
  filter(ecoregion == "Total", tipo == "Current") %>%
  select(metric, area_km2)

# Plot
ggplot(
  df_sens %>% filter(ecoregion == "Total", tipo != "Current"),
  aes(x = ano, y = area_km2, color = metric, shape = metric, group = metric)
) +
  geom_hline(
    data = df_current_sens,
    aes(yintercept = area_km2, color = metric),
    linetype = "dashed", linewidth = 0.6
  ) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  geom_line(position = position_dodge(0.5), linewidth = 0.4) +
  facet_wrap(~ tipo, scales = "free_x", nrow = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = expression("Suitable area (km"^2*")"), color = NULL, shape = NULL) +
  scale_shape_manual(values = c(
    "Max Sorensen"    = 16,
    "Equal Sens Spec" = 17,
    "Max Sens Spec"   = 15,
    "LPT"             = 19
  ))+
  theme_bw() +
  theme(
    legend.position = "top",
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    strip.text      = element_text(face = "bold")
  )

png("sup3_sens_test.png", height = 20, width=20, unit="cm",res=600)
dev.off()

