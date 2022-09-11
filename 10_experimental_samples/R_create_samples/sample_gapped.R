# *****************************************************************************
# R Script for preparing clustered samples with spatial gaps for subsequent 
# analysis related to the manuscript "Dealing with clustered samples for 
# assessing map accuracy by cross-validation".
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

# create outfolders if they don't exist
if(!dir.exists(outfolder))
  dir.create(outfolder)

if(!dir.exists(paste0(outfolder, "/clusterGapped")))
  dir.create(paste0(outfolder, "/clusterGapped"))

# download data from https://doi.org/10.5281/zenodo.6513429
# read the data of the study area
msk <- rast(file.path(infolder, "TOTmask.tif"))
AGBstack <- rast(file.path(infolder, "AGBstack.tif"))
OCSstack <- rast(file.path(infolder, "OCSstack.tif"))

# read the Ploton et al. gridded sample locations
# downloaded from: https://doi.org/10.6084/m9.figshare.11865450
Plo1km <- raster::raster(file.path(infolder, "Ploton-et-al_AGB-1-km.tif"))[[1]]

# resize and shift to make it fit the study area in Europe
Plo025 <- raster::disaggregate(Plo1km, 4, filename = file.path(infolder, "Ploton_etal_025km.tif"), 
                       overwrite=T)
Plo025 <- as(Plo025, "SpatRaster")
tmp <- ext(Plo025)
ext(Plo025) <- c(tmp[1], 2*tmp[2]-tmp[1], tmp[3], 2*tmp[4]-tmp[3])

# coordinate shift
e1 <- ext(AGBstack)
c1 <- c(mean(e1[1:2]), mean(e1[3:4]))
e2 <- ext(Plo025)
c2 <- c(mean(e2[1:2]), mean(e2[3:4]))

PloShift <- shift(Plo025, c1[1]-c2[1], c1[2]-c2[2], 
                  filename="data/PloShift.tif", overwrite=T)

# set the CRS
crs(PloShift) <- crs(AGBstack)

# convert to points
PloPts <- as.points(PloShift)
PloPts$Plo_1 <- NULL
writeVector(PloPts, "../data/PloPts.shp", overwrite = T)

# randomly shift up to 300 km shift in x and 70 km in y direction
# and sample
i <- 0
set.seed(1234567)
while(i < n_samp){
  xShift <- 500*round(runif(1, -3e5, 3e5)/500)
  yShift <- 500*round(runif(1, -7e4, 7e4)/500)
  
  PloTmp <- shift(PloPts, xShift, yShift)
  tst <- extract(msk, PloTmp)[,2]
  tst <- which(tst == 1)
  if(length(tst) >= 5000){
    i <- i + 1
    idx <- sample(tst, 5000)
    AGBdata <- extract(AGBstack, PloTmp[idx])
    OCSdata <- extract(OCSstack, PloTmp[idx])
    AGBdata$glc2017 <- factor(AGBdata$glc2017, levels=1:8)
    OCSdata$glc2017 <- factor(OCSdata$glc2017, levels=1:8)
    
    pts_sf <- sf::st_as_sf(PloTmp[idx])
    # pts_df <- sfheaders::sf_to_df(pts_sf)
    # pts <- pts_df[,c("x", "y")]
    
    fname <- paste0("AGBdata", sprintf("%03d", i), ".Rdata")
    save(AGBdata, file=file.path(outfolder, "clusterGapped", fname))
    fname <- paste0("OCSdata", sprintf("%03d", i), ".Rdata")
    save(OCSdata, file=file.path(outfolder, "clusterGapped", fname))
    
    # save geo-coordinates to extra file, so normal way is not disturbed
    coord_name <- paste0(sprintf("%03d", i), "_coords", ".Rdata")
    save(pts_sf, file=file.path(outfolder, "clusterGapped", coord_name))
  }
}

