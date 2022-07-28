setwd("~/iloek_job/wadoux/deBruin_add_nndm/")
save_to <- "~/iloek_job/wadoux/deBruin_add_nndm/samples"
samples_root <- "~/iloek_job/wadoux/investigate_spatial_validation/debruin/samples/"
samples <- 700

# while doing AGB, take care of pts coordinate files as well
for (method in c("clusterGapped", "clusterMedium", "simpleRandom", "clusterStrong", "regular")) {
  realizations <- list.files(file.path(samples_root, method), glob2rx("AGBdata*.Rdata"))
  realizat_OCS <- list.files(file.path(samples_root, method), glob2rx("OCSdata*.Rdata"))
  pts_files <- list.files(file.path(samples_root, method), glob2rx("*coords.Rdata"))
  for (i in 1:length(realizations)) {
    load(file.path(samples_root, method, realizations[i]))
    load(file.path(samples_root, method, realizat_OCS[i]))
    load(file.path(samples_root, method, pts_files[i]))
    row_numbers <- sample(1:nrow(AGBdata), samples)
    AGBdata <- AGBdata[row_numbers,]
    OCSdata <- OCSdata[row_numbers,]
    pts <- pts[row_numbers,]
    if("ID" %in% names(AGBdata)) {AGBdata <- AGBdata[,! names(AGBdata) %in% c("ID")]}
    if("ID" %in% names(OCSdata)) {OCSdata <- OCSdata[,! names(OCSdata) %in% c("ID")]}
    save(AGBdata, file = file.path(save_to, method, realizations[i]))
    save(OCSdata, file = file.path(save_to, method, realizat_OCS[i]))
    save(pts, file = file.path(save_to, method, pts_files[i]))
  }
}
