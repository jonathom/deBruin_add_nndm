# *****************************************************************************
# R Script implementing conventional random f-fold cross-validation.  
# Related to the paper "Dealing with clustered samples for assessing map 
# accuracy by cross-validation".
# Contact: Sytze de Bruin, Wageningen University, Laboratory of Geo-information
# Science and Remote Sensing, email: sytze.debruin@wur.nl
# May 3, 2022
# *****************************************************************************

# sleep for random time so multiple jobs dont take the same job out of the csv
# Sys.sleep(round(runif(1, min = 1, max = 240)))

# ****** load required library *******
.libPaths("~/R")
library(ranger)
source("/home/j/j_bahl03/R/CAST/R/global_validation.R")
source("/home/j/j_bahl03/R/CAST/R/nndm.R")
library(lwgeom)
library(sf)
library(raster)
library(caret)
library(parallel)

# ************ GLOBALS ***************
infolder <- "~/deBruin_add_nndm/10_experimental_samples/samples/sampled_1500"
outfolder <- "~/deBruin_add_nndm/10_experimental_samples/CVresults/"
# outfolder <- "~/iloek_job/wadoux/investigate_spatial_validation/debruin/CVresults"
datafolder <- "~/deBruin_add_nndm/data"
folder_name <- "nndm_sampled1000_samps_1500_mint_0"

n_CV <- 1

# workaround because of pipe: source scripts from CAST package
# manual copy/paste of "sampleFromArea" with old pipe

sampleFromArea <- function(modeldomain, samplesize, type,variables,sampling){
  
  ##### Distance to prediction locations:
  # regularly spread points (prediction locations):
  # see https://edzer.github.io/OGH21/
  if(inherits(modeldomain, "Raster")){
    if(samplesize>raster::ncell(modeldomain)){
      samplesize <- raster::ncell(modeldomain)
      message(paste0("samplesize for new data shouldn't be larger than number of pixels.
              Samplesize was reduced to ",raster::ncell(modeldomain)))
    }
    modeldomainextent <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(modeldomain)))
  }else{
    modeldomainextent <- modeldomain
  }
  
  sf::sf_use_s2(FALSE)
  sf::st_as_sf(modeldomainextent) %>%
    sf::st_transform(4326) -> bb
  methods::as(bb, "Spatial") %>%
    sp::spsample(n =samplesize, type = sampling)  %>%
    sf::st_as_sfc() %>%
    sf::st_set_crs(4326) -> predictionloc
  
  
  predictionloc <- sf::st_as_sf(predictionloc)
  
  
  if(type == "feature"){
    predictionloc <- sf::st_as_sf(raster::extract(modeldomain, predictionloc, df = TRUE, sp = TRUE))
    predictionloc <- na.omit(predictionloc)
  }
  
  return(predictionloc)
  
}

###############################################

# create outfolders if they don't exist
if(!dir.exists(outfolder))
  dir.create(outfolder)

if(!dir.exists(paste0(outfolder, folder_name)))
  dir.create(paste0(outfolder, folder_name))

# ************ FUNCTIONS ***************

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

nndmCV <- function(smpl, number, variate) {
  
  fname <- paste0(variate, "data", sprintf("%03d", number), ".Rdata")
  f_in <- file.path(infolder,smpl,fname)
  load(f_in)
  
  RMSE <- numeric(n_CV)
  
  # load sample file containing coordinates "pts_sf"
  load(file.path(infolder,smpl,paste0(sprintf("%03d", number), "_coords", ".Rdata")))
  
  agb_raster <- raster::raster(file.path(datafolder, "agb.tif")) # load agb raster
  
  folds <- list()
  
  for(i_CV in 1:n_CV) {
    
    #st_as_sf(raster::rasterToPoints(agb_raster[[1]], spatial = TRUE))
    
    # NNDM
    # folds have nothing to do with AGBdata
    # samplesize 1000 is standard
    nndm <- nndm(tpoints = pts_sf, modeldomain = agb_raster, samplesize = 1000)
    # save(nndm, file="./nndm.Rdata")
    
    # Evaluate RF model using NDM CV
    trainControl_NNDM <- trainControl(method = "cv",
                                      index=nndm$indx_train,
                                      indexOut=nndm$indx_test,
                                      savePredictions = "final") # save predictions final to avoid writing CV myself
    
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
    
    RMSE[i_CV] <- global_validation(mod_NNDM)["RMSE"][[1]]
    folds <- append(folds, nndm)
    
  }
  
  fname  <-  paste0(variate, "_", smpl, sprintf("%03d", number), ".Rdata")
  f_out  <- file.path(outfolder,folder_name, fname)
  save(RMSE, folds, file=f_out)
}

# ************ CALL THE FUNCTIONS ************ 
# nndmCV(smpl = row$sample, number = row$i_samp, variate = row$variate)

samples <- c("clusterMedium", "clusterStrong", "clusterGapped", "regular", 
               "simpleRandom")
n_samp <- 10
cores <- 20

# mclapply(seq(n_samp), function(i) {
# 4 is missing from all results so redo run nr 4
# mclapply(list(4), function(i) {
#   for(smpl in samples) {
#     nndmCV(smpl = smpl, number = i, variate = "AGB")
#     nndmCV(smpl, i, "OCS")
#   }
# }, mc.cores = cores)

# do one first
mclapply(c("AGB", "OCS"), function(vari) {
  for (smpl in samples) {
    nndmCV(smpl=smpl, number=1, variate=vari)
  }
}, mc.cores=2)
