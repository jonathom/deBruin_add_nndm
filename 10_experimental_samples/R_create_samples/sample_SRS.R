# *****************************************************************************
# R Script for preparing simple random samples (SRS) for subsequent analysis 
# related to the paper "Dealing with clustered samples for assessing map 
# accuracy by cross-validation".
# Contact: Sytze de Bruin, Wageningen University, Laboratory of Geo-information
# Science and Remote Sensing, email: sytze.debruin@wur.nl
# May 3, 2022
# *****************************************************************************

# ****** load required library *******
library(terra)
library(sf)


# ************ GLOBALS ***************
infolder  <- "../../data" # use data in upper dir
outfolder <- "../samples/original"
n_samp    <- 10  # number of sample replicates


# ********* load input data **********
# download data from https://doi.org/10.5281/zenodo.6513429

msk <- rast(file.path(infolder, "TOTmask.tif"))

AGBstack <- rast(file.path(infolder, "AGBstack.tif"))
OCSstack <- rast(file.path(infolder, "OCSstack.tif"))


# create outfolders if they don't exist
if(!dir.exists(outfolder))
  dir.create(outfolder)

if(!dir.exists(paste0(outfolder, "/simpleRandom")))
  dir.create(paste0(outfolder, "/simpleRandom"))

# ******* create the samples ********
set.seed(1234567)

for (i in 1:n_samp){
  idx <- as.integer(spatSample(msk, 5000, method="random", 
                                na.rm=T, cells=T)[,1])
  AGBdata <- extract(AGBstack, idx) # the df contains x/y values converted by 1000 for whatever reason
  OCSdata <- extract(OCSstack, idx)
  AGBdata$glc2017 <- factor(AGBdata$glc2017, levels=1:8)
  OCSdata$glc2017 <- factor(OCSdata$glc2017, levels=1:8)
  
  pts <- xyFromCell(msk, idx)
  
  # add conversion to sf
  pts_df <- as.data.frame(pts)
  pts_sf <- sf::st_as_sf(pts_df, coords = c("x", "y"))
  st_crs(pts_sf) <- st_crs(AGBstack)
  
  fname <- paste0("AGBdata", sprintf("%03d", i), ".Rdata")
  save(AGBdata, file=file.path(outfolder, "simpleRandom", fname))
  fname <- paste0("OCSdata", sprintf("%03d", i), ".Rdata")
  save(OCSdata, file=file.path(outfolder, "simpleRandom", fname))
  
  coord_name <- paste0(sprintf("%03d", i), "_coords", ".Rdata")
  save(pts_sf, file=file.path(outfolder, "simpleRandom", coord_name))
}
