# *****************************************************************************
# R Script for preparing clustered samples for subsequent analysis related to
# the manuscript "Dealing with clustered samples for assessing map accuracy by 
# cross-validation".
# Contact: Sytze de Bruin, Wageningen University, Laboratory of Geo-information
# Science and Remote Sensing, email: sytze.debruin@wur.nl
# May 3, 2022
# *****************************************************************************

# ****** load required library *******
library(terra)


# ************ GLOBALS ***************
infolder  <- "../../data"
outfolder <- "../samples/original"
n_samp    <- 10  # number of sample replicates

# ********* load input data **********
# download data from https://doi.org/10.5281/zenodo.6513429

strata <- vect(file.path(infolder,"strata.shp"))

msk <- rast(file.path(infolder, "TOTmask.tif"))
crs(strata) <- crs(msk)

AGBstack <- rast(file.path(infolder, "AGBstack.tif"))
OCSstack <- rast(file.path(infolder, "OCSstack.tif"))

# create outfolders if they don't exist
if(!dir.exists(outfolder))
  dir.create(outfolder)

if(!dir.exists(paste0(outfolder, "/clusterMedium")))
  dir.create(paste0(outfolder, "/clusterMedium"))

if(!dir.exists(paste0(outfolder, "/clusterStrong")))
  dir.create(paste0(outfolder, "/clusterStrong"))


# 1st case: 50% sample from 20% of the area
set.seed(1234567)

for (i in 1:n_samp){
  idx <- sample.int(100, 20)
  ndx <- setdiff(1:100, idx)
  
  msk1 <- mask(msk, strata[idx,])
  msk2 <- mask(msk, strata[ndx,])
  
  sub <- sample(cells(msk1), 2500)
  sub <- c(sub, sample(cells(msk2), 2500))
  
  pts <- xyFromCell(msk, sub)
  # add conversion to sf
  pts_df <- as.data.frame(pts)
  pts_sf <- sf::st_as_sf(pts_df, coords = c("x", "y"))
  st_crs(pts_sf) <- st_crs(AGBstack)
  
  AGBdata <- extract(AGBstack, sub) # here, xcoord and ycoord are passed along from AGBstack, no CRS just raster cell coords
  # AGBdata2 <- extract(AGBstack, pts, xy=T) # to export with actual LAEA coords, same order
  OCSdata <- extract(OCSstack, sub)
  AGBdata$glc2017 <- factor(AGBdata$glc2017, levels=1:8)
  OCSdata$glc2017 <- factor(OCSdata$glc2017, levels=1:8)
  
  fname <- paste0("AGBdata", sprintf("%03d", i), ".Rdata")
  save(AGBdata, file=file.path(outfolder, "clusterMedium", fname))
  fname <- paste0("OCSdata", sprintf("%03d", i), ".Rdata")
  save(OCSdata, file=file.path(outfolder, "clusterMedium", fname))
  
  # save geo-coordinates to extra file, so normal way is not disturbed
  coord_name <- paste0(sprintf("%03d", i), "_coords", ".Rdata")
  save(pts_sf, file=file.path(outfolder, "clusterMedium", coord_name))
  
}


# 2nd case: 90% sample from 10% of the area
set.seed(1234567)

for (i in 1:n_samp){
  idx <- sample.int(100, 10)
  ndx <- setdiff(1:100, idx)
  
  msk1 <- mask(msk, strata[idx,])
  msk2 <- mask(msk, strata[ndx,])
  
  sub <- sample(cells(msk1), 4500)
  sub <- c(sub, sample(cells(msk2), 500))
  pts <- xyFromCell(msk, sub)
  # add conversion to sf
  pts_df <- as.data.frame(pts)
  pts_sf <- sf::st_as_sf(pts_df, coords = c("x", "y"))
  st_crs(pts_sf) <- st_crs(AGBstack)
  
  AGBdata <- extract(AGBstack, sub)
  OCSdata <- extract(OCSstack, sub)
  AGBdata$glc2017 <- factor(AGBdata$glc2017, levels=1:8)
  OCSdata$glc2017 <- factor(OCSdata$glc2017, levels=1:8)
  
  fname <- paste0("AGBdata", sprintf("%03d", i), ".Rdata")
  save(AGBdata, file=file.path(outfolder, "clusterStrong", fname))
  fname <- paste0("OCSdata", sprintf("%03d", i), ".Rdata")
  save(OCSdata, file=file.path(outfolder, "clusterStrong", fname))
  
  # save geo-coordinates to extra file, so normal way is not disturbed
  coord_name <- paste0(sprintf("%03d", i), "_coords", ".Rdata")
  save(pts_sf, file=file.path(outfolder, "clusterStrong", coord_name))
}
