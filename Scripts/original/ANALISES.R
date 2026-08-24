library(exactextractr)
library(terra)
library(sf)
library(tidyverse)

# ------- SHAPEFILE -------
sp_region_sf <- read_sf(choose.files())
sp_region_sf <- st_set_crs(sp_region_sf, 4326)

# ------- RASTER -------
wmean<-rast(choose.files())
ssp245<-rast(choose.files())
ssp585<-rast(choose.files())




### MapBiomas
### fogo ####
fogo<-list.files(".", pattern="tif")
fogo<-rast(fogo)

fogo_r<- resample(fogo, wmean, method = "near")
fogo_r[fogo_r>0]<-1
plot(fogo_r)

##negativo
fogo_pres<-list()

for(i in 1:4){
  fogo_pres[[i]]<-wmean-fogo_r[[i]] 
  fogo_pres[[i]][fogo_pres[[i]]<0]<-0
}
names(fogo)
fogo_pres<-rast(fogo_pres)
names(fogo_pres)<-c("1995", "2005", "2015", "2022")
plot(fogo_pres)



f<-fogo_pres
plot(f)
f<-c(wmean,f,ssp245, ssp585)

anos <- c("current","1995", "2005", "2015", "2022", "SSP245","SSP585")
names(f)<-anos
plot(f)

###Salvando
fogo_95<-f[[2]]
fogo_05<-f[[3]]
fogo_15<-f[[4]]
fogo_22<-f[[5]]

writeRaster(fogo_95, "presente_fogo95.tif")
writeRaster(fogo_05, "presente_fogo05.tif")
writeRaster(fogo_15, "presente_fogo15.tif")
writeRaster(fogo_22, "presente_fogo22.tif")

threshold
f_bin<-f
plot(f_bin)

f_bin[f_bin<0.5126028]<-0
f_bin[f_bin>=0.5126028]<-1
plot(f_bin)

###Salvando
fogo_95b<-f_bin[[2]]
fogo_05b<-f_bin[[3]]
fogo_15b<-f_bin[[4]]
fogo_22b<-f_bin[[5]]

writeRaster(fogo_95b, "bin_presente_fogo95.tif")
writeRaster(fogo_05b, "bin_presente_fogo05.tif")
writeRaster(fogo_15b, "bin_presente_fogo15.tif")
writeRaster(fogo_22b, "bin_presente_fogo22.tif")


valores2<-exact_extract(f_bin,sp_region_sf,
                       coverage_area=T,
                       fun="sum")
valores2<-valores2/1e+6
valores2$ecoregiao<-sp_region_sf$NOMPAISA
names(valores2)<-gsub("sum.","",names(valores2))

##LULC####
cod<-read.csv2("cod_mapbiomas.csv", h=T)
antro<-cod[c(14:30,32:34),]

lulc<-list.files(".", pattern="tif")
lulc<-rast(lulc)
lulc_r<- resample(lulc, wmean, method = "near")

lulc_r_bin<-lulc_r
lulc_r_bin[lulc_r_bin %in% antro$Class_ID]<-1
lulc_r_bin[lulc_r_bin!=1]<-0
plot(lulc_r_bin)

lulc_pres<-list()
for(i in 1:4){
  lulc_pres[[i]]<-wmean-lulc_r_bin[[i]] 
  lulc_pres[[i]][lulc_pres[[i]]<0]<-0
}
lulc_pres<-rast(lulc_pres)
plot(lulc_pres)
names(lulc_pres)<-c("1995", "2005", "2015", "2022")

lulc_95<-lulc_pres[[1]]
lulc_05<-lulc_pres[[2]]
lulc_15<-lulc_pres[[3]]
lulc_22<-lulc_pres[[4]]
writeRaster(lulc_95, "presente_lulc95.tif")
writeRaster(lulc_05, "presente_lulc05.tif")
writeRaster(lulc_15, "presente_lulc15.tif")
writeRaster(lulc_22, "presente_lulc22.tif")


lulc_bin<-lulc_pres
plot(lulc_bin)

lulc_bin[lulc_bin<0.5126028]<-0
lulc_bin[lulc_bin>=0.5126028]<-1
plot(lulc_bin)

lulc_95b<-lulc_bin[[1]]
lulc_05b<-lulc_bin[[2]]
lulc_15b<-lulc_bin[[3]]
lulc_22b<-lulc_bin[[4]]
writeRaster(lulc_95b, "bin_presente_lulc95.tif")
writeRaster(lulc_05b, "bin_presente_lulc05.tif")
writeRaster(lulc_15b, "bin_presente_lulc15.tif")
writeRaster(lulc_22b, "bin_presente_lulc22.tif")


valores3<-exact_extract(lulc_bin,sp_region_sf,
                        coverage_area=T,
                        fun="sum")
valores3<-valores3/1e+6
valores3$ecoregiao<-sp_region_sf$NOMPAISA
names(valores3)<-gsub("sum.","",names(valores3))

###Combinado ####
##Efeito combinado####

timeserie<-c("1995", "2005", "2015", "2022")
combined_pres <- list()

for (timeserie in timeserie) {
  r_fogo <- fogo_pres[[which(timeserie == timeserie)]]
  r_lulc <- lulc_pres[[timeserie]]
  
  combined <- min(r_fogo, r_lulc)
  combined_pres[[timeserie]] <- combined
}
combined_pres<-rast(combined_pres)
names(combined_pres)

plot(combined_pres)

for(i in 1:4){
  writeRaster(combined_pres[[i]], filename = paste("combined_",names(combined_pres[[i]]),".tif", sep=""))
}


combined_bin<-combined_pres
plot(combined_bin)

combined_bin[combined_bin<0.5126028]<-0
combined_bin[combined_bin>=0.5126028]<-1
plot(combined_bin)

for(i in 1:4){
  writeRaster(combined_bin[[i]], filename = paste("bin_combined_",names(combined_bin[[i]]),".tif", sep=""))
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



val<-as.data.frame(val)

sum_eco<-val %>% group_by(eco,name) %>% 
  mutate(value=sum(value))

write.csv(val, "area-values.csv",row.names = F)
