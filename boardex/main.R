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
Sys.setenv("GCS_DEFAULT_BUCKET"="corporate-finance-data")
gcs_auth(email = "wakaflocka12211@gmail.com")
gcs_global_bucket("corporate-finance-data-2")
#####GCE Boardex#####
bucket_info <- gcs_get_bucket("corporate-finance-data-2")
objects <- gcs_list_objects("corporate-finance-data-2")
print(objects)
#####Check if Data Folder Exists#####
if(file.exists(file.path(wd,"data","boardex_gai.RData"))){
  load(file.path(wd,"data","boardex_gai.RData"))
}
if(skp_splt_ind == F){
  if(!file.exists(file.path(wd,"data"))){
    dir.create(file.path(wd,"data"))
  }
  if(!file.exists(file.path(wd,"data","boardex_individual_profiles_employment.dta"))){
    gcs_get_object(objects$name[grep("Individual Profile Employment",objects$name)[1]], saveToDisk = file.path(wd,"data","boardex_individual_profiles_employment.dta"))
    #unzip(file.path(wd,"data","boardex_individual_profiles_empolyment.zip"))
    #file.rename(file.path(wd,"data","Individual Profile Employment 1933-01-2019-02.dta"),file.path(wd,"data","boardex_individual_profiles_employment.dta"))
  }
  boardex = read_dta(file.path(wd,"data","boardex_individual_profiles_employment.dta"))
  print(dim(boardex))
  #####SETWD######
  source(file.path(wd,"functions","expandpanalbaordex.R"))
  source(file.path(wd,"functions","split_index.R"))
  source(file.path(wd,"functions","split_rolename.R"))
  source(file.path(wd,"functions","split_sector.R"))
  ##############
  list.of.sequences <- list()
  steps = 100
  jmp = round(nrow(boardex)/steps)
  for(i in 1:(steps-1)){
    list.of.sequences[[i]] = seq(1+(i-1)*jmp,1+i*jmp)
  }
  list.of.sequences[[steps]] = seq(1+(steps-1)*jmp,nrow(boardex))
  list.of.dt <- list()
  i = 1
  for(setofnum in list.of.sequences){
    expanded.dt = expandemployment(temp = boardex[setofnum,])
    print(length(expanded.dt))
    list.of.dt[[i]] <- split_list(expanded.dt)
    save.image(file.path(wd,"data/","boardex_gai.RData"))
    i = i+1
    print(i)
    print(length(list.of.sequences))
    print(Sys.time() - a)
  }
}
list.of.dt <- future_lapply(list.of.dt, bind_rows)
print("The sub list were binded")
df <- list.of.dt %>% bind_rows() %>% splt_role_name()
print("the final dataframe has the size")
print(dim(df))
df_factors <- gai_func(rd = T, dt = df)
upload_gcs_from_file(authfile="~/gai/fluted-century-372700-7fbd134783e5.json",bucket = "corporate-finance-data",pth = "~",nm = "gai_factors2.rds")





