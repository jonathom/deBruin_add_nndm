setwd("~/iloek_job/wadoux/deBruin_add_nndm/")
save_to <- "~/iloek_job/wadoux/deBruin_add_nndm/samples"
samples_root <- "~/iloek_job/wadoux/investigate_spatial_validation/debruin/samples/"
samples <- 700

for (method in c("clusterGapped", "clusterMedium", "simpleRandom", "clusterStrong", "regular")) {
  realizations <- list.files(file.path(samples_root, method), glob2rx("AGBdata*.Rdata"))
  for (i in 1:length(realizations)) {
    load(file.path(samples_root, method, realizations[i]))
    AGBdata <- AGBdata[sample(1:nrow(AGBdata), samples),]
    if("ID" %in% names(AGBdata)) {AGBdata <- AGBdata[,! names(AGBdata) %in% c("ID")]}
    save(AGBdata, file = file.path(save_to, method, realizations[i]))
  }
}

for (method in c("clusterGapped", "clusterMedium", "simpleRandom", "clusterStrong", "regular")) {
  realizations <- list.files(file.path(samples_root, method), glob2rx("OCSdata*.Rdata"))
  for (i in 1:length(realizations)) {
    load(file.path(samples_root, method, realizations[i]))
    OCS <- OCSdata[sample(1:nrow(OCSdata), samples),]
    if("ID" %in% names(OCSdata)) {OCSdata <- OCSdata[,! names(OCSdata) %in% c("ID")]}
    save(OCSdata, file = file.path(save_to, method, realizations[i]))
  }
}
