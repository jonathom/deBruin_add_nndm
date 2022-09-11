save_to <- "~/iloek_job/wadoux/deBruin_add_nndm/10_experimental_samples/samples/sampled_700"
samples_root <- "~/iloek_job/wadoux/deBruin_add_nndm/10_experimental_samples/samples/original/"
samples <- 700

if(!dir.exists(paste0(save_to, "/simpleRandom")))
  dir.create(paste0(save_to, "/simpleRandom"))
if(!dir.exists(paste0(save_to, "/clusterGapped")))
  dir.create(paste0(save_to, "/clusterGapped"))
if(!dir.exists(paste0(save_to, "/clusterMedium")))
  dir.create(paste0(save_to, "/clusterMedium"))
if(!dir.exists(paste0(save_to, "/clusterStrong")))
  dir.create(paste0(save_to, "/clusterStrong"))
if(!dir.exists(paste0(save_to, "/regular")))
  dir.create(paste0(save_to, "/regular"))

for (method in c("clusterGapped", "clusterMedium", "simpleRandom", "clusterStrong", "regular")) {
  real_AGB <- list.files(file.path(samples_root, method), glob2rx("AGBdata*.Rdata"))
  real_OCS <- list.files(file.path(samples_root, method), glob2rx("OCSdata*.Rdata"))
  real_pts <- list.files(file.path(samples_root, method), glob2rx("*_coords.Rdata"))
  for (i in 1:length(real_AGB)) {
    load(file.path(samples_root, method, real_AGB[i]))
    load(file.path(samples_root, method, real_OCS[i]))
    load(file.path(samples_root, method, real_pts[i]))
    
    row_numbers <- sample(1:nrow(AGBdata), samples)
    
    AGBdata <- AGBdata[row_numbers,] # this is correct however I found slight inconsistencies in the coords while testing for clusterGapped
    OCSdata <- OCSdata[row_numbers,]
    pts_sf     <- pts_sf[row_numbers,] 
    # this was called "pts" before although some folders contained only "pts_sf"
    # before they were all named "pts"
    # delete ID column
    if("ID" %in% names(AGBdata)) {AGBdata <- AGBdata[,! names(AGBdata) %in% c("ID")]}
    if("ID" %in% names(OCSdata)) {OCSdata <- OCSdata[,! names(OCSdata) %in% c("ID")]}
    
    save(AGBdata, file = file.path(save_to, method, real_AGB[i]))
    save(OCSdata, file = file.path(save_to, method, real_OCS[i]))
    save(pts_sf, file = file.path(save_to, method, real_pts[i]))
    rm(pts_sf)
  }
}
