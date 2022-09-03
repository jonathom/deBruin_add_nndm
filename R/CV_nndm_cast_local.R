# ****** load required library *******
library(ranger)
library(NNDM)
library(caret)
library(CAST)
library(sf)
library(raster)
library(parallel)

# ************ GLOBALS ***************
infolder <- "./samples"
outfolder <- "./CVresults/"
# outfolder <- "~/iloek_job/wadoux/investigate_spatial_validation/debruin/CVresults"
datafolder <- "./data"
folder_name <- "nndm_"

# csv_file <- file.path(outfolder, folder_name, "nndm_processing.csv")
# runs <- read.csv(csv_file)
# i <- 0
# found <- FALSE
# while (found == FALSE && i < 301) {
#   i <- i+1
#   row <- runs[i,]
#   if (row$job == 0) {
#     # print(paste(row, i))
#     row$job <- 1
#     runs[i,] <- row
#     found <- TRUE
#   }
# }
# 
# write.csv(runs, file = csv_file, row.names = FALSE)

# 20000 phi and 0.5 min train
n <- 700 # usually 5000
n_CV <- 3

# create outfolders if they don't exist
if(!dir.exists(outfolder))
  dir.create(outfolder)

if(!dir.exists(paste0(outfolder, folder_name)))
  dir.create(paste0(outfolder, folder_name))

df <- data.frame(smpl=NA, number=NA, variate=NA, nndm=NA, cast=NA)

nndmCV <- function(smpl, number, variate) {
  
  # smpl <- "simpleRandom"
  # number <- 10
  # variate <- "AGB"
  
  fname <- paste0(variate, "data", sprintf("%03d", number), ".Rdata")
  f_in <- file.path(infolder,smpl,fname)
  load(f_in)
  
  # RMSE <- numeric(n_CV)
  
  # load sample file containing coordinates
  load(file.path(infolder,smpl,paste0(sprintf("%03d", number), "_coords", ".Rdata")))
  if(class(pts)[1] != "sf") {
    sample_df <- as.data.frame(pts)
    sample_sf <- st_as_sf(sample_df, coords = c("x", "y"))
  } else {
    sample_sf <- pts
  }
  
  agb_raster <- raster::raster(file.path(datafolder, "agb.tif")) # load agb raster
  
  folds <- list()


    
  raster_subset <- sampleRandom(agb_raster, n, sp=T) # subset raster
  raster_sf <- st_as_sf(raster_subset) # as sf
  sample_subset <- st_cast(st_sample(sample_sf, n), to = "POINT") # subset sample
  st_crs(sample_subset) <- st_crs(agb_raster) # set crs
  
  #st_as_sf(raster::rasterToPoints(agb_raster[[1]], spatial = TRUE))
  nndm <- NNDM::nndm(tpoints = sample_subset, ppoints = raster_sf, phi = 20000, min_train = 0.5) # 20000 in old runs
  nndm_cast <- CAST::nndm(tpoints = sample_subset, modeldomain = agb_raster, sampling = "random", min_train = 0.5)
  # save(nndm, file="./nndm.Rdata")
  
  # Evaluate RF model using NDM CV
  trainControl_NNDM <- trainControl(method = "cv",
                                    index=nndm$indx_train,
                                    indexOut=nndm$indx_test,
                                    savePredictions = "final") # save predictions final to avoid writing CV myself
  
  trainControl_CAST <- trainControl(method = "cv",
                                    index=nndm_cast$indx_train,
                                    indexOut=nndm_cast$indx_test,
                                    savePredictions = "final")
  
  paramGrid <-  data.frame(mtry = 2, min.node.size = 5, splitrule = "variance")
  
  if (variate == "AGB") {
    training_data <- AGBdata; rf_form <- agb~.
  } else {
    training_data <- OCSdata; rf_form <- ocs~.
  }
  
  # next: execute this and see where it goes from there
  mod_NNDM <- train(rf_form,
                    method = "ranger",
                    trControl = trainControl_NNDM,
                    tuneGrid = paramGrid, 
                    data = training_data)
  
  mod_CAST <- train(rf_form,
                    method = "ranger",
                    trControl = trainControl_CAST,
                    tuneGrid = paramGrid, 
                    data = training_data)
  
  RMSE_nndm <- global_validation(mod_NNDM)["RMSE"][[1]]
  RMSE_cast <- global_validation(mod_CAST)["RMSE"][[1]]
  folds <- append(folds, c(nndm, nndm_cast))
  
  df <- data.frame(smpl=NA, number=NA, variate=NA, type=NA, RMSE=NA)
  df[1,] <- c(smpl, number, variate, "nndm", RMSE_nndm)
  df[2,] <- c(smpl, number, variate, "cast", RMSE_cast)
  return(df)
}

# ************ CALL THE FUNCTIONS ************ 
# nndmCV(smpl = row$sample, number = row$i_samp, variate = row$variate)

samples <- c(#"clusterMedium", "clusterStrong", "clusterGapped", "regular", 
               "simpleRandom")
n_samp <- 7
cores <- 3

res <- mclapply(seq(n_samp), function(i) {
    nndmCV(smpl = "simpleRandom", number = i, variate = "AGB")
}, mc.cores=cores)

df <- as.data.frame(do.call("rbind", res))
df$ex <- NA

for (j in seq(1,2*n_samp,2)) {
  fname <- paste0("AGB_simpleRandom", sprintf("%03d", j), ".Rdata")
  load(paste0("./CVresults/exhaustive/", fname))
  df[j,6] <- RMSE
  df[j+1,6] <- RMSE
}

df$RMSE <- as.numeric(df$RMSE)
df$rel <- 100 * (df$RMSE - df$ex)/ df$RMSE

ggplot(data=df) +
  geom_boxplot(aes(x=variate, y=RMSE, color=type))
  
######
for (i in 1) {
  # the folder with the "old" nndm-package nndms
  fname <- paste0("AGB_simpleRandom", sprintf("%03d", i), ".Rdata")
  load(paste0("./CVresults/nndm/", fname)) 
  agb_raster <- raster::raster(file.path(datafolder, "agb.tif")) # load agb raster

  # load data for new model building
  fname <- paste0("AGB", "data", sprintf("%03d", i), ".Rdata")
  f_in <- file.path(infolder,"simpleRandom",fname)
  load(f_in)
  
  # load sample file containing coordinates
  load(file.path(infolder,"simpleRandom",paste0(sprintf("%03d", i), "_coords", ".Rdata")))
  if(class(pts)[1] != "sf") {
    sample_df <- as.data.frame(pts)
    sample_sf <- st_as_sf(sample_df, coords = c("x", "y"))
  } else {
    sample_sf <- pts
  }
  rm(sample_df)
  
  # sample_subset <- st_cast(st_sample(sample_sf, n), to = "POINT") # subset sample
  st_crs(sample_sf) <- st_crs(agb_raster) # set crs
  
  nndm_cast <- CAST::nndm(tpoints = sample_sf, modeldomain = agb_raster, sampling = "random", min_train = 0.5)

  trainControl_CAST <- trainControl(method = "cv",
                                    index=nndm_cast$indx_train,
                                    indexOut=nndm_cast$indx_test,
                                    savePredictions = "final")
  
  paramGrid <-  data.frame(mtry = 2, min.node.size = 5, splitrule = "variance")
  
  training_data <- AGBdata; rf_form <- agb~.
  
  mod_CAST <- train(rf_form,
                    method = "ranger",
                    trControl = trainControl_CAST,
                    tuneGrid = paramGrid, 
                    data = training_data)
  
  RMSE_cast <- global_validation(mod_CAST)["RMSE"][[1]] # maybe this is the problem?
  
  # folds vs nndm_cast and RMSE vs RMSE_cast
  nndm_orig <- folds[1:7]
  
  ex_or <- list()
  ex_ca <- list()
  
  for (j in 1:700) {
    ex_or <- append(ex_or, length(nndm_orig$indx_exclude[[j]]))
    ex_ca <- append(ex_ca, length(nndm_cast$indx_exclude[[j]]))
  }
  
  length(unique(ex_or))
  length(unique(ex_ca))
  max(as.numeric(ex_or))
  max(as.numeric(ex_ca))
  
  
  sumSquares <- function(ref, pred){
    muref <- mean(ref, na.rm=T)
    SSR <- sum((ref - pred)^2)
    SST <- sum((ref - muref)^2)
    return(c(SSR, SST))
  }
  
  err_fu <- function(obs, pred){
    rmse <- sqrt(mean((obs-pred)^2))
    muref <- mean(obs)
    SSR <- sum((obs - pred)^2)
    SST <- sum((obs - muref)^2)
    mec <- 1 - SSR/SST
    me <- mean(obs - pred)
    list(me = me, rmse = rmse, mec = mec)
  }
  
  err_fu(mod_CAST$pred$obs, mod_CAST$pred$pred)$rmse # same as with global_validation
  
  
  
}

####################################################
df <- data.frame(uni_o=NA, uni_c=NA, r_o=NA, r_c=NA)

for (i in 1:10) {
  # the folder with the "old" nndm-package nndms
  fname <- paste0("AGB_simpleRandom", sprintf("%03d", i), ".Rdata")
  load(paste0("./CVresults/nndm/", fname))
  nndm_orig <- folds[1:7]
  rmse_orig <- RMSE[1]
  load(paste0("./CVresults/nndm_cast/", fname))
  nndm_cast <- folds[1:7]
  rmse_cast <- RMSE[1]
  
  ex_or <- list()
  ex_ca <- list()
  
  for (j in 1:700) {
    ex_or <- append(ex_or, length(nndm_orig$indx_exclude[[j]]))
    ex_ca <- append(ex_ca, length(nndm_cast$indx_exclude[[j]]))
  }
  
  df[i,] <- c(length(unique(ex_or)), length(unique(ex_ca)), rmse_orig, rmse_cast)
  
}

# so it seems CAST::nndm is working as expected (seeing how it produces CVs with more excluded points, always around 100)
# it also seems its not the validation, I'm using CAST::global_validation but it is the same as calculating the RMSE with the pred/obs in the returned model
# phi (NNDM::nndm) also doesn't seem to influence the pattern
# min_train also doesn't, as it seems