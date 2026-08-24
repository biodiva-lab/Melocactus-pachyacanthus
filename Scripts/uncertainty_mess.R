library(terra)
library(sf)
library(ggplot2)
library(patchwork)


##ecoregions
sp_region_sf <-read_sf("ecoregions/eco_melocactus.shp")


ls<-list.files(".",pattern = "tif$")

##Extrapolation
mess_r<-ls[grepl("MESS_",ls)]

mess_245<-rast(mess_r[grepl("245",mess_r)])
names(mess_245)<-c("IPSL-CM6A-LR","MIROC6","MRI-ESM2-0")

mess_585<-rast(mess_r[grepl("585",mess_r)])
names(mess_585)<-c("IPSL-CM6A-LR","MIROC6","MRI-ESM2-0")
ls
mess_cur<-rast("MESS_cur.tif")

#Uncertainty

unc<-rast(ls[grepl("uncertainty", ls)])
names(unc)<-c("SSP2-4.5", "SSP5-8.5")

plot(mess_245)


##PLOTS

rast_to_df_named <- function(r, nome) {
  as.data.frame(r, xy = TRUE) %>%
    setNames(c("x", "y", "value")) %>%
    mutate(scenario = nome)
}
region_layer <- geom_sf(
  data        = sp_region_sf,
  fill        = NA,
  color       = "black",
  linewidth   = 0.4,
  inherit.aes = FALSE
)
# ------- scale function MESS -------
scale_mess_custom <- function(min_val, max_val) {
  scale_fill_gradientn(
    colors = c("#C52603","#FFAAAA", "white","#30678D"),
    values = scales::rescale(c(min_val, 0, 0.001, max_val)),
    limits = c(min_val, max_val),
    name   = "MESS"
  )
}

# ------- plot function MESS -------
plot_mess <- function(df, titulo) {
  min_val <- min(df$value, na.rm = TRUE)
  max_val <- max(df$value, na.rm = TRUE)
  
  ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    region_layer +
    scale_mess_custom(min_val, max_val) +
    facet_wrap(~ scenario, nrow = 1) +
    labs(title=titulo)+
    theme_void() +
    theme(
      strip.text       = element_text(face = "bold", size = 9),
      axis.text        = element_text(size = 6),
      axis.title       = element_blank(),
      plot.title       = element_text(face = "bold", size = 10),
      legend.position  = "right",
      panel.background = element_rect(fill = "gray95")
    )
}


# ------- PLOT UNCERTAINTY -------
plot_unc <- function(r, titulo) {
  df <- rast_to_df_named(r, titulo)
  min_val <- min(df$value, na.rm = TRUE)
  max_val <- max(df$value, na.rm = TRUE)
  
  ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    region_layer +
    scale_fill_gradientn(
      colors = c("white", "#FDE725", "#FB8322", "#C52603"),
      limits = c(min_val, max_val),
      name   = "Uncertainty\n(SD)"
    ) +
    facet_wrap(~ scenario, nrow = 1) +
    labs(title = titulo) +
    theme_void() +
    theme(
      strip.text       = element_blank(),
      axis.text        = element_text(size = 6),
      axis.title       = element_blank(),
      plot.title       = element_text(face = "bold", size = 10),
      legend.position  = "right",
      panel.background = element_rect(fill = "gray95")
    )
}

# ------- DATAFRAMES -------
df_mess_245 <- bind_rows(
  rast_to_df_named(mess_245[["IPSL-CM6A-LR"]], "IPSL-CM6A-LR"),
  rast_to_df_named(mess_245[["MIROC6"]],       "MIROC6"),
  rast_to_df_named(mess_245[["MRI-ESM2-0"]],   "MRI-ESM2-0")
)

df_mess_585 <- bind_rows(
  rast_to_df_named(mess_585[["IPSL-CM6A-LR"]], "IPSL-CM6A-LR"),
  rast_to_df_named(mess_585[["MIROC6"]],       "MIROC6"),
  rast_to_df_named(mess_585[["MRI-ESM2-0"]],   "MRI-ESM2-0")
)

df_mess_cur <- rast_to_df_named(mess_cur, "Current")

# ------- PLOTS -------
p_A <- plot_mess(df_mess_245, "A")
p_B <- plot_mess(df_mess_585, "B")
p_C <- plot_mess(df_mess_cur, "C")
p_D <- plot_unc(unc[["SSP2-4.5"]], "Uncertainty SSP2-4.5")
p_E <- plot_unc(unc[["SSP5-8.5"]], "Uncertainty SSP5-8.5")

# ------- combine -------
p_A /p_B

p_D + p_E


png("sup1_mess.png", height = 20, width=30, unit="cm",res=600)
p_A /p_B
dev.off()

png("sup2_uncertainty.png", height = 15, width=30, unit="cm",res=600)
p_D + p_E
dev.off()

