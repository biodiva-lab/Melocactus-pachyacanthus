

###Modelling Melocactus pachycanthus

library(flexsdm)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)

d_prin<-getwd()

setwd(d_prin)

dir()
melocactus<-read.csv("cleaned_occ_gbif.csv", h=T, sep=";")

melocactus$pr_ab<-1

melo_bckp<-melocactus


##Getting rasters
###Abrir pasta com rasters
dir()
setwd("cur_env")
all_vars<-list.files(".", pattern="tif$")
all_vars
##
somevar<-rast(all_vars)
names(somevar)
##Get ecorregion
setwd(d_prin)
sp_region<-vect("ecoregions/eco_melocactus.shp")

###Occurrence filter
names(melocactus)
melocactus<-occfilt_geo(
  melocactus,
  x="decimalLongitude",
  y="decimalLatitude",
  env_layer=somevar,
  method = c('cellsize', factor = '1'),
  prj = "+proj=longlat +datum=WGS84",
  reps = 20
)

distance<-st_as_sf(melocactus,coords = c("decimalLongitude"
                                        ,"decimalLatitude"),
                                        crs =crs(somevar) )


dist<-as.data.frame(st_distance(distance))

ca <- calib_area(
  data = melocactus,
  x="decimalLongitude",
  y="decimalLatitude",
  method = c('buffer', width=max(dist)),
  crs = crs(somevar)
)

plot(
  sp_region,
  col = "gray80",
  legend = FALSE,
  axes = FALSE,
  main = "Calibration area and occurrences"
)
plot(ca, add = TRUE)
points(melocactus[, c("decimalLongitude","decimalLatitude")], col = "black", pch = 16)



# Sample the same number of species presences
set.seed(42)
psa <- sample_pseudoabs(
  data = melocactus,
  x="decimalLongitude",
  y="decimalLatitude",
  n = sum(melocactus$pr_ab), # selecting number of pseudo-absence points that is equal to number of presences
  method = "random",
  rlayer = somevar,
  calibarea = ca
)

# Visualize species presences and pseudo-absences
plot(
  sp_region,
  col = "gray80",
  legend = FALSE,
  axes = FALSE,
  main = "Presence = yellow, Pseudo-absence = black"
)
plot(ca, add = TRUE)
points(psa[, c("decimalLongitude","decimalLatitude")], cex = 0.8, pch = 16, col = "black") # Pseudo-absences
points(melocactus[, c("decimalLongitude","decimalLatitude")], col = "yellow", pch = 16, cex = 1.5) # Presences


# Bind a presences and pseudo-absences
melocactus_pa <- bind_rows(melocactus, psa)
melocactus_pa # Presence-Pseudo-absence database


set.seed(10)

# Repeated K-fold method
melocactus_pa2 <- part_random(
  data = melocactus_pa,
  pr_ab = "pr_ab",
  method = c(method = "rep_kfold", folds = 3, replicates = 10)
)

#####################################################
############ ENVIROMENTAL###########################
melocactus_pa3 <-
  sdm_extract(
    data = melocactus_pa2,
    x = "decimalLongitude",
    y = "decimalLatitude",
    env_layer = somevar,
    variables = c("bio_4", "bio_12", "bio_10", "bio_15" )
  )
names(melocactus_pa3)
melocactus_pa3<-melocactus_pa3 %>% select(
  -gbifID,-countryCode,-basisOfRecord
)

names(somevar)


##Model training
eglm <-
  esm_glm(
    data = melocactus_pa3,
    response = "pr_ab",
    predictors = c("bio_4", "bio_12", "bio_10", "bio_15"),
    partition = ".part",
    thr = "max_sorensen"
  )


esvm <- esm_svm(
  data = melocactus_pa3,
  response = "pr_ab",
  predictors = c("bio_4", "bio_12", "bio_10", "bio_15"),
  partition = ".part",
  thr = "max_sorensen"
)

egam <-
  esm_gam(
    data = melocactus_pa3,
    response = "pr_ab",
    predictors = c("bio_4", "bio_12", "bio_10", "bio_15"),
    partition = ".part",
    thr = "max_sorensen"
  )

set.seed(42)

bg <- sample_background(
  data = melocactus,
  x = "decimalLongitude",
  y = "decimalLatitude",
  n = 10000,
  method = "random",
  rlayer = somevar,
  calibarea = ca) %>%
  sdm_extract(
    data = .,
    x = "decimalLongitude",
    y = "decimalLatitude",
    env_layer = somevar) %>% 
  part_random(
    data = .,
    pr_ab = "pr_ab",
    method = c(method = "rep_kfold", folds = 3, replicates = 10)
  )


emax <-
  esm_max(
    data = melocactus_pa3,
    response = "pr_ab",
    predictors = c("bio_4", "bio_12", "bio_10", "bio_15"),
    partition = ".part",
    background = bg,
    thr = "max_sorensen"
  )


####Map prediction

eglm_pred <- sdm_predict(
  models = eglm,
  pred = somevar,
  predict_area = sp_region
)
plot(eglm_pred[[1]])


esvm_pred <- sdm_predict(
  models = esvm,
  pred = somevar,
  predict_area = sp_region
)

egam_pred <- sdm_predict(
  models = egam,
  pred = somevar,
  predict_area = sp_region
)

emax_pred <- sdm_predict(
  models = emax,
  pred = somevar,
  predict_area = sp_region
)


######Validacao

setwd(d_prin)
setwd("Result")

merge_df <- sdm_summarize(models = list(eglm, esvm, egam, emax))

write.csv(merge_df, "evaluation_sd.csv", row.names = F)


evaluation<-
  merge_df %>% dplyr::select(
    model,
    thr_value = thr_value,
    AUC = AUC_mean,
    TSS = TSS_mean,
    SORENSEN = SORENSEN_mean,
    BOYCE = BOYCE_mean,
    IMAE = IMAE_mean
  )
evaluation


###Ensemble

ensemble<-c(eglm_pred[[1]],
            emax_pred[[1]],
            esvm_pred[[1]],
            egam_pred[[1]])
plot(ensemble)

wmean<-weighted.mean(ensemble, w=evaluation$SORENSEN)
plot(wmean)
plot(ca, add=T)

###Evaluating ensemble
ensemble_ext<-sdm_extract(
  data = melocactus_pa,
  x = "decimalLongitude",
  y = "decimalLatitude",
  env_layer = wmean)

p<-ensemble_ext[ensemble_ext$pr_ab==1,"sum"]
a<-ensemble_ext[ensemble_ext$pr_ab==0,"sum"]

ensemble_eval<-sdm_eval(p=p$sum,
                        a=a$sum,
                        thr=c("max_sorensen","equal_sens_spec","max_sens_spec","lpt"))

evaluation_ens<-
  ensemble_eval%>% dplyr::select(thr_value,
    AUC,TSS,SORENSEN,BOYCE,IMAE)
evaluation_ens$model<-"ensemble_wmean"

evaluation<-rbind(evaluation, evaluation_ens)

write.csv(evaluation, "evaluation_clean.csv", row.names = F)

writeRaster(wmean, filename = "ESM_ensemble_cur.tif")

somevar
##Extrapolation
library(predicts)

ref_train<-as.data.frame(melocactus_pa3)
ref_train<-ref_train[,c(17,16,18,15)]

names(ref_train)==names(somevar)

cur_mess<-predicts::mess(x=somevar,v=ref_train)
cur_mess
plot(cur_mess)

writeRaster(cur_mess, filename = "MESS_cur.tif")


###Projection

# Directory with future projections data
fut_dir <- "C:/Users/Thiie/OneDrive/Pesquisa/BIODIVA/Modelagem_Melocactus/fut_env"
scenarios <- dir(fut_dir)

# Empty list for store ensembles
ensemble_fut <- list()

for (scenario in scenarios) {
  
  # Load scenario raster
  scenario_path <- file.path(fut_dir, scenario)
  fut_vars_files <- list.files(scenario_path, pattern = "tif$", full.names = TRUE)
  fut_vars <- rast(fut_vars_files)
  
  # Predictions
  eglm_pred_f <- sdm_predict(models = eglm, pred = fut_vars, predict_area = sp_region)
  emax_pred_f <- sdm_predict(models = emax, pred = fut_vars, predict_area = sp_region)
  esvm_pred_f <- sdm_predict(models = esvm, pred = fut_vars, predict_area = sp_region)
  egam_pred_f <- sdm_predict(models = egam, pred = fut_vars, predict_area = sp_region)
  
  # Ensemble
  ensemble_f <- c(eglm_pred_f[[1]], emax_pred_f[[1]], esvm_pred_f[[1]], egam_pred_f[[1]])
  wmean_fut <- weighted.mean(ensemble_f, w = evaluation$SORENSEN)
  
  # 
  ensemble_fut[[scenario]] <- wmean_fut
  
  message("OK: ", scenario)
}

names(ensemble_fut)

plot(rast(ensemble_fut))

for(i in 1:length(ensemble_fut)){
  writeRaster(ensemble_fut[[i]], filename = paste("projection_",names(ensemble_fut)[[i]],".tif", sep = ""))
}

# Mean of scenarios
ssp245_mean <- mean(c(ensemble_fut[["IPSL-CM6A-LR_ssp245_2041-2060"]],
                      ensemble_fut[["MIROC6_ssp245_2041-2060"]],
                      ensemble_fut[["MRI-ESM2-0_ssp245_2041-2060"]]))

ssp585_mean <- mean(c(ensemble_fut[["IPSL-CM6A-LR_ssp585_2041-2060"]],
                      ensemble_fut[["MIROC6_ssp585_2041-2060"]],
                      ensemble_fut[["MRI-ESM2-0_ssp585_2041-2060"]]))
plot(wmean)
plot(ssp245_mean)
plot(ssp585_mean)

writeRaster(ssp245_mean, filename = "ESM_ssp245.tif")
writeRaster(ssp585_mean, filename = "ESM_ssp585.tif")

# Uncertainty of scenarios
##SSP245
ssp245_u<-c(ensemble_fut[["IPSL-CM6A-LR_ssp245_2041-2060"]],
            ensemble_fut[["MIROC6_ssp245_2041-2060"]],
            ensemble_fut[["MRI-ESM2-0_ssp245_2041-2060"]])

ssp245_sd <- app(ssp245_u, fun = sd)   # cell by cell standard deviation
plot(ssp245_sd)
##SSP585
ssp585_u<-c(ensemble_fut[["IPSL-CM6A-LR_ssp585_2041-2060"]],
            ensemble_fut[["MIROC6_ssp585_2041-2060"]],
            ensemble_fut[["MRI-ESM2-0_ssp585_2041-2060"]])

ssp585_sd <- app(ssp585_u, fun = sd)   # cell by cell standard deviation
plot(ssp585_sd)

writeRaster(ssp245_sd, filename = "uncertainty_ssp245.tif")
writeRaster(ssp585_sd, filename = "uncertainty_ssp585.tif")


###Extrapolation of scenarios

fut_mess<-list()
for (scenario in scenarios) {
  
  # Load scenario raster
  scenario_path <- file.path(fut_dir, scenario)
  fut_vars_files <- list.files(scenario_path, pattern = "tif$", full.names = TRUE)
  fut_vars <- rast(fut_vars_files)
  
  # Extrapolation
  fut_extra<-predicts::mess(x=fut_vars,v=ref_train)
  fut_mess[[scenario]]<-fut_extra
  
  message("OK: ", scenario)
}

plot(rast(fut_mess))

for(i in 1:length(fut_mess)){
  writeRaster(fut_mess[[i]], filename = paste("MESS_",names(fut_mess)[[i]],".tif", sep = ""))
}

library(ggplot2)
library(patchwork)
library(colorspace)
library(sf)

# ------- PALETA TURBO 
A <- c(0.18995,0.19483,0.19956,0.20415,0.2086,0.21291,0.21708,0.22111,0.225,0.22875,0.23236,0.23582,0.23915,0.24234,0.24539,0.2483,0.25107,0.25369,0.25618,0.25853,0.26074,0.2628,0.26473,0.26652,0.26816,0.26967,0.27103,0.27226,0.27334,0.27429,0.27509,0.27576,0.27628,0.27667,0.27691,0.27701,0.27698,0.2768,0.27648,0.27603,0.27543,0.27469,0.27381,0.27273,0.27106,0.26878,0.26592,0.26252,0.25862,0.25425,0.24946,0.24427,0.23874,0.23288,0.22676,0.22039,0.21382,0.20708,0.20021,0.19326,0.18625,0.17923,0.17223,0.16529,0.15844,0.15173,0.14519,0.13886,0.13278,0.12698,0.12151,0.11639,0.11167,0.10738,0.10357,0.10026,0.0975,0.09532,0.09377,0.09287,0.09267,0.0932,0.09451,0.09662,0.09958,0.10342,0.10815,0.11374,0.12014,0.12733,0.13526,0.14391,0.15323,0.16319,0.17377,0.18491,0.19659,0.20877,0.22142,0.23449,0.24797,0.2618,0.27597,0.29042,0.30513,0.32006,0.33517,0.35043,0.36581,0.38127,0.39678,0.41229,0.42778,0.44321,0.45854,0.47375,0.48879,0.50362,0.51822,0.53255,0.54658,0.56026,0.57357,0.58646,0.59891,0.61088,0.62233,0.63323,0.64362,0.65394,0.66428,0.67462,0.68494,0.69525,0.70553,0.71577,0.72596,0.7361,0.74617,0.75617,0.76608,0.77591,0.78563,0.79524,0.80473,0.8141,0.82333,0.83241,0.84133,0.8501,0.85868,0.86709,0.8753,0.88331,0.89112,0.8987,0.90605,0.91317,0.92004,0.92666,0.93301,0.93909,0.94489,0.95039,0.9556,0.96049,0.96507,0.96931,0.97323,0.97679,0.98,0.98289,0.98549,0.98781,0.98986,0.99163,0.99314,0.99438,0.99535,0.99607,0.99654,0.99675,0.99672,0.99644,0.99593,0.99517,0.99419,0.99297,0.99153,0.98987,0.98799,0.9859,0.9836,0.98108,0.97837,0.97545,0.97234,0.96904,0.96555,0.96187,0.95801,0.95398,0.94977,0.94538,0.94084,0.93612,0.93125,0.92623,0.92105,0.91572,0.91024,0.90463,0.89888,0.89298,0.88691,0.88066,0.87422,0.8676,0.86079,0.8538,0.84662,0.83926,0.83172,0.82399,0.81608,0.80799,0.79971,0.79125,0.7826,0.77377,0.76476,0.75556,0.74617,0.73661,0.72686,0.71692,0.7068,0.6965,0.68602,0.67535,0.66449,0.65345,0.64223,0.63082,0.61923,0.60746,0.5955,0.58336,0.57103,0.55852,0.54583,0.53295,0.51989,0.50664,0.49321,0.4796)
B <- c(0.07176,0.08339,0.09498,0.10652,0.11802,0.12947,0.14087,0.15223,0.16354,0.17481,0.18603,0.1972,0.20833,0.21941,0.23044,0.24143,0.25237,0.26327,0.27412,0.28492,0.29568,0.30639,0.31706,0.32768,0.33825,0.34878,0.35926,0.3697,0.38008,0.39043,0.40072,0.41097,0.42118,0.43134,0.44145,0.45152,0.46153,0.47151,0.48144,0.49132,0.50115,0.51094,0.52069,0.5304,0.54015,0.54995,0.55979,0.56967,0.57958,0.5895,0.59943,0.60937,0.61931,0.62923,0.63913,0.64901,0.65886,0.66866,0.67842,0.68812,0.69775,0.70732,0.7168,0.7262,0.73551,0.74472,0.75381,0.76279,0.77165,0.78037,0.78896,0.7974,0.80569,0.81381,0.82177,0.82955,0.83714,0.84455,0.85175,0.85875,0.86554,0.87211,0.87844,0.88454,0.8904,0.896,0.90142,0.90673,0.91193,0.91701,0.92197,0.9268,0.93151,0.93609,0.94053,0.94484,0.94901,0.95304,0.95692,0.96065,0.96423,0.96765,0.97092,0.97403,0.97697,0.97974,0.98234,0.98477,0.98702,0.98909,0.99098,0.99268,0.99419,0.99551,0.99663,0.99755,0.99828,0.99879,0.9991,0.99919,0.99907,0.99873,0.99817,0.99739,0.99638,0.99514,0.99366,0.99195,0.98999,0.98775,0.98524,0.98246,0.97941,0.9761,0.97255,0.96875,0.9647,0.96043,0.95593,0.95121,0.94627,0.94113,0.93579,0.93025,0.92452,0.91861,0.91253,0.90627,0.89986,0.89328,0.88655,0.87968,0.87267,0.86553,0.85826,0.85087,0.84337,0.83576,0.82806,0.82025,0.81236,0.80439,0.79634,0.78823,0.78005,0.77181,0.76352,0.75519,0.74682,0.73842,0.73,0.7214,0.7125,0.7033,0.69382,0.68408,0.67408,0.66386,0.65341,0.64277,0.63193,0.62093,0.60977,0.59846,0.58703,0.57549,0.56386,0.55214,0.54036,0.52854,0.51667,0.50479,0.49291,0.48104,0.4692,0.4574,0.44565,0.43399,0.42241,0.41093,0.39958,0.38836,0.37729,0.36638,0.35566,0.34513,0.33482,0.32473,0.31489,0.3053,0.29599,0.28696,0.27824,0.26981,0.26152,0.25334,0.24526,0.2373,0.22945,0.2217,0.21407,0.20654,0.19912,0.19182,0.18462,0.17753,0.17055,0.16368,0.15693,0.15028,0.14374,0.13731,0.13098,0.12477,0.11867,0.11268,0.1068,0.10102,0.09536,0.0898,0.08436,0.07902,0.0738,0.06868,0.06367,0.05878,0.05399,0.04931,0.04474,0.04028,0.03593,0.03169,0.02756,0.02354,0.01963,0.01583)
C <- c(0.23217,0.26149,0.29024,0.31844,0.34607,0.37314,0.39964,0.42558,0.45096,0.47578,0.50004,0.52373,0.54686,0.56942,0.59142,0.61286,0.63374,0.65406,0.67381,0.693,0.71162,0.72968,0.74718,0.76412,0.7805,0.79631,0.81156,0.82624,0.84037,0.85393,0.86692,0.87936,0.89123,0.90254,0.91328,0.92347,0.93309,0.94214,0.95064,0.95857,0.96594,0.97275,0.97899,0.98461,0.9893,0.99303,0.99583,0.99773,0.99876,0.99896,0.99835,0.99697,0.99485,0.99202,0.98851,0.98436,0.97959,0.97423,0.96833,0.9619,0.95498,0.94761,0.93981,0.93161,0.92305,0.91416,0.90496,0.8955,0.8858,0.8759,0.86581,0.85559,0.84525,0.83484,0.82437,0.81389,0.80342,0.79299,0.78264,0.7724,0.7623,0.75237,0.74265,0.73316,0.72393,0.715,0.70599,0.69651,0.6866,0.67627,0.66556,0.65448,0.64308,0.63137,0.61938,0.60713,0.59466,0.58199,0.56914,0.55614,0.54303,0.52981,0.51653,0.50321,0.48987,0.47654,0.46325,0.45002,0.43688,0.42386,0.41098,0.39826,0.38575,0.37345,0.3614,0.34963,0.33816,0.32701,0.31622,0.30581,0.29581,0.28623,0.27712,0.26849,0.26038,0.2528,0.24579,0.23937,0.23356,0.22835,0.2237,0.2196,0.21602,0.21294,0.21032,0.20815,0.2064,0.20504,0.20406,0.20343,0.20311,0.2031,0.20336,0.20386,0.20459,0.20552,0.20663,0.20788,0.20926,0.21074,0.2123,0.21391,0.21555,0.21719,0.2188,0.22038,0.22188,0.22328,0.22456,0.2257,0.22667,0.22744,0.228,0.22831,0.22836,0.22811,0.22754,0.22663,0.22536,0.22369,0.22161,0.21918,0.2165,0.21358,0.21043,0.20706,0.20348,0.19971,0.19577,0.19165,0.18738,0.18297,0.17842,0.17376,0.16899,0.16412,0.15918,0.15417,0.1491,0.14398,0.13883,0.13367,0.12849,0.12332,0.11817,0.11305,0.10797,0.10294,0.09798,0.0931,0.08831,0.08362,0.07905,0.07461,0.07031,0.06616,0.06218,0.05837,0.05475,0.05134,0.04814,0.04516,0.04243,0.03993,0.03753,0.03521,0.03297,0.03082,0.02875,0.02677,0.02487,0.02305,0.02131,0.01966,0.01809,0.0166,0.0152,0.01387,0.01264,0.01148,0.01041,0.00942,0.00851,0.00769,0.00695,0.00629,0.00571,0.00522,0.00481,0.00449,0.00424,0.00408,0.00401,0.00401,0.0041,0.00427,0.00453,0.00486,0.00529,0.00579,0.00638,0.00705,0.0078,0.00863,0.00955,0.01055)

turbo_hex <- hex(sRGB(cbind(A, B, C)))



###Open data###

wmean  <- rast(choose.files())
ssp245 <- rast(choose.files())
ssp585 <- rast(choose.files())

# Converter rasters para dataframe
rast_to_df <- function(r) {
  as.data.frame(r, xy = TRUE) %>%
    setNames(c("x", "y", "suitability"))
}


df_present <- rast_to_df(wmean)
df_ssp245  <- rast_to_df(ssp245)
df_ssp585  <- rast_to_df(ssp585)

# Converter shape para sf
#sp_region_sf <- st_as_sf(sp_region)
sp_region_sf <-read_sf(choose.files())
# ------- ESCALA E CAMADAS

color_scale <- scale_fill_gradientn(
  colors = rev(turbo_hex),
  limits = c(0, 1),
  name = "Suitability"
)

region_layer <- geom_sf(
  data = sp_region_sf,
  fill = NA,
  color = "black",
  linewidth = 0.4,
  inherit.aes = FALSE
)

# ------- PLOTS -------
(p_present <- ggplot(df_present, aes(x = x, y = y, fill = suitability)) +
  geom_raster() +
  region_layer +
  color_scale +
  coord_sf() +
  labs(title = "Current") +
  theme_bw() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title   = element_blank(),
    legend.position = "none"
  ))

p_ssp245 <- ggplot(df_ssp245, aes(x = x, y = y, fill = suitability)) +
  geom_raster() +
  region_layer +
  color_scale +
  coord_sf() +
  labs(title = "SSP2-4.5") +
  theme_bw() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title   = element_blank(),
    legend.position = "none"
  )

p_ssp585 <- ggplot(df_ssp585, aes(x = x, y = y, fill = suitability)) +
  geom_raster() +
  region_layer +
  color_scale +
  coord_sf() +
  labs(title = "SSP5-8.5") +
  theme_bw() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title   = element_blank(),
    legend.position = "right"
  )

# ------- COMBINAR -------

p_present + p_ssp245 + p_ssp585 +
  plot_layout(guides = "collect") +
  theme(legend.position = "right")

eval<-read.csv("evaluation_clean.csv", h=T)

thresholds<-eval[eval$model=="ensemble_wmean", 1:2]
thresholds$metric<-c("Max Sorensen","Equal Sens Spec","Max Sens Spec","LPT")

# ------- BINARIZAR DATAFRAMES -------
bin_rast_to_df <- function(df) {
  df %>% mutate(suitability = ifelse(suitability >= threshold, "1", NA))
}

df_present_bin <- bin_rast_to_df(df_present)
df_ssp245_bin  <- bin_rast_to_df(df_ssp245)
df_ssp585_bin  <- bin_rast_to_df(df_ssp585)

# ------- ESCALA BINÁRIA -------
color_scale_bin <- scale_fill_manual(
  values   = c("1" = "#31AFF5"),
  na.value = "gray",
  na.translate=F,
  name     = "Suitability",
  labels   = c("1" = "Presence", "NA" = "Abscense")
)

# ------- PLOTS BINÁRIOS -------
p_present_bin <- ggplot(df_present_bin, aes(x = x, y = y, fill = suitability)) +
  geom_raster() +
  region_layer +
  color_scale_bin +
  coord_sf() +
  labs(title = "Current (binary)") +
  theme_bw() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title      = element_blank(),
    legend.position = "none"
  )

p_ssp245_bin <- ggplot(df_ssp245_bin, aes(x = x, y = y, fill = suitability)) +
  geom_raster() +
  region_layer +
  color_scale_bin +
  coord_sf() +
  labs(title = "SSP2-4.5 (binary)") +
  theme_bw() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title      = element_blank(),
    legend.position = "none"
  )

p_ssp585_bin <- ggplot(df_ssp585_bin, aes(x = x, y = y, fill = suitability)) +
  geom_raster() +
  region_layer +
  color_scale_bin +
  coord_sf() +
  labs(title = "SSP5-8.5 (binary)") +
  theme_bw() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title      = element_blank(),
    legend.position = "right"
  )

# ------- COMBINAR -------
(p_present + p_ssp245 + p_ssp585) /
  (p_present_bin + p_ssp245_bin + p_ssp585_bin) +
  plot_layout(guides = "collect") +
  theme(legend.position = "right")

png("fig2_bin.png", height = 20, width=30, unit="cm",res=600)
dev.off()





####REABRINDO####
wmean<-rast(choose.files())
ssp245<-rast(choose.files())
ssp585<-rast(choose.files())

# Binarizar rasters
wmean_bin <- wmean
wmean_bin[wmean_bin < threshold] <- NA
wmean_bin[wmean_bin >= threshold] <- 1

ssp245_bin <- ssp245
ssp245_bin[ssp245_bin < threshold] <- NA
ssp245_bin[ssp245_bin >= threshold] <- 1

ssp585_bin <- ssp585
ssp585_bin[ssp585_bin < threshold] <- NA
ssp585_bin[ssp585_bin >= threshold] <- 1

# Garantir CRS compatível
sp_region_sf <- st_set_crs(sp_region_sf, 4326)

# Função para calcular área por ecorregião
calc_area_eco <- function(r, eco_sf) {
  
  results <- list()
  
  for (i in 1:nrow(eco_sf)) {
    
    # Recortar raster pela ecorregião
    eco_vect  <- vect(eco_sf[i, ])
    r_crop    <- crop(r, eco_vect)
    r_mask    <- mask(r_crop, eco_vect)
    
    # Calcular área
    area_km2 <- global(cellSize(r_mask, unit = "km") * r_mask,
                       fun = "sum", na.rm = TRUE)$sum
    
    results[[i]] <- data.frame(
      ecoregion = eco_sf$NOMPAISA[i],
      area_km2  = round(area_km2, 2)
    )
  }
  
  bind_rows(results)
}

# Calcular para cada cenário
eco_current <- calc_area_eco(wmean_bin,  sp_region_sf) %>% mutate(scenario = "Current")
eco_ssp245  <- calc_area_eco(ssp245_bin, sp_region_sf) %>% mutate(scenario = "SSP2-4.5")
eco_ssp585  <- calc_area_eco(ssp585_bin, sp_region_sf) %>% mutate(scenario = "SSP5-8.5")

# Combinar
df_eco_areas <- bind_rows(eco_current, eco_ssp245, eco_ssp585)
df_eco_areas
plot(sp_region_sf$geometry, add=T)
plot(ssp245_bin)
plot(wmean_bin)

df_eco_areas %>% group_by(scenario) %>% 
  summarise(sum(area_km2))


