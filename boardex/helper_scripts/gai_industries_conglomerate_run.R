args = commandArgs(trailingOnly=TRUE)
# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}else if(length(args) > 0 & length(args) < 3){
  wd = args[1]
  authfile = args[2]
}else{
  wd = args[1]
  authfile = args[2]
  skp_splt_ind = as.logical(args[3])
}
list.of.packages <- c("tidyr", "stringi", "dplyr", "purrr","future.apply","rlang",
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
Sys.setenv("GCS_DEFAULT_BUCKET"="corporate-finance-data-2")
gcs_auth(email = "wakaflocka12211@gmail.com")
gcs_global_bucket("corporate-finance-data-2")
#####GCE Boardex#####
bucket_info <- gcs_get_bucket("corporate-finance-data-2")
objects <- gcs_list_objects("corporate-finance-data-2")
print(objects)
#####Check if Data Folder Exists#####
gcs_get_object(nm, saveToDisk = file.path(wd,"data","gai_factors_data_segments.csv"))
#dt = fread(file.path(wd,"data","boardex_individual_profiles_employment.csv"))
#source(file.path(wd,"functions","gai_industries_conglomerate.R"))
#temp2 <- merge(temp,gai_conglomerate_sectors(dftemp = temp), by = c("directorid","year"))
#temp2 <- temp2 %>% select(c(gvkey, directorname, directorid, companyid, year, rolename, minceoyear,
#                            ceo_dummy, Countpastfirms, Countpastpositions, Countsegments, max_segments,
#                            conglomerate_exp, count_industries)) %>% arrange(directorid, year)