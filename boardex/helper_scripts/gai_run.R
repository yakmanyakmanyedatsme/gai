args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}else if(length(args)){
  wd = args[1]
  authfile = args[2]
}
list.of.packages <- c("tidyr", "stringi", "dplyr", "purrr","future.apply","rlang","parallel",
                      "data.table", "haven", "readxl", "lubridate", "googleCloudStorageR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "https://cloud.r-project.org/")
lapply(list.of.packages, require, character.only = T)
#####SETWD######
setwd(wd)
print("the setwd worked")
######GCE Authentication#######
a = Sys.time()
Sys.setenv("GCS_AUTH_FILE" = authfile)
Sys.setenv("GCS_DEFAULT_BUCKET"="corporate-finance-data 1")
gcs_auth(email = "wakaflocka12211@gmail.com")
gcs_global_bucket("corporate-finance-data 1")
#####GCE Boardex#####
source(file.path(wd,"gai","boardex","functions","gai_industries_conglomerate.R"))
source(file.path(wd,"gai","boardex","functions","gai.R"))
bucket_info <- gcs_get_bucket("corporate-finance-data 1")
objects <- gcs_list_objects("corporate-finance-data 1")
print(objects)
if(!file.exists(file.path(wd,"boardex_gai_stage2_full.RData"))){
  gcs_get_object("boardex_gai_stage2_full.RData", saveToDisk = file.path(wd,"boardex_gai_stage2_full.RData"))
}
load(file.path(wd,"boardex_gai_stage2_full.RData"))
df_factors <- gai_func(rd = F, dt = gai_comp_seg) %>% distinct()
saveRDS(df_factors,"factors_int_full.rds")
gcs_upload(file = "factors_int_full.rds",predefinedAcl = "bucketLevel",upload_type = "simple")

