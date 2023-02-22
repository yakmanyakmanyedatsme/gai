args = commandArgs(trailingOnly=TRUE)
# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}else if(length(args) > 0 & length(args) < 3){
  wd = args[1]
  authfile = args[2]
}else if(length(args) == 3){
  wd = args[1]
  authfile = args[2]
  skp_splt_ind = as.logical(args[3])
}else{
  wd = args[1]
  authfile = args[2]
  skp_splt_ind = as.logical(args[3])
  brk = as.logical(args[4])
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
if(file.exists(file.path(wd,"data","boardex_gai.RData"))){
  load(file.path(wd,"data","boardex_gai.RData"))
}
if(skp_splt_ind == F){
  if(!file.exists(file.path(wd,"data"))){
    dir.create(file.path(wd,"data"))
  }
  if(!file.exists(file.path(wd,"Individual Profile Employment 1933-01-2019-02.dta"))){
    gcs_get_object("Individual Profile Employment 1933-01-2019-02.dta", saveToDisk = file.path(wd,"Individual Profile Employment 1933-01-2019-02.dta"))
  }
  
  if(!file.exists(file.path(wd,"compustat_segments.dta"))){
    gcs_get_object("Annual_Compensation_Execucomp.dta", saveToDisk = file.path(wd,"compustat_segments.dta"))
  }
  if(!file.exists(file.path(wd,"1970_2022_Compustat.dta"))){
    gcs_get_object("1970_2022_Compustat.dta", saveToDisk = file.path(wd,"1970_2022_Compustat.dta"))
  }
  if(!file.exists(file.path(wd,"compustat_global_ids.rds"))){
    gcs_get_object("compustat_global_ids.rds", saveToDisk = file.path(wd,"compustat_global_ids.rds"))
  }
  
  #########Read in data##########
  #######GAI Data##############
  compustat_global_ids = readRDS(file.path(wd,"compustat_global_ids.rds"))
  comp <- read_dta(file.path(wd,"1970_2022_Compustat.dta"))
  boardex = read_dta(file.path(wd,"boardex_individual_profiles_employment 1933-01-2019-02.dta.dta"))
  boardex = boardex %>% filter(Orgtype == "Quoted")
  compustat_segments = read_dta(file.path(wd,"compustat_segments.dta"))
  print(dim(boardex))
  #######Compustat Global#######
  compustat_global_ids <- compustat_global_ids %>% rename(year = fyear)
  compustat_global_ids <- compustat_global_ids %>% select(gvkey, conm, isin, year, sic, sich)
  #######Compustat segments#######
  compustat_segments <- compustat_segments %>% mutate(year = year(datadate)) %>% select(gvkey, year, snms) 
  compustat_segments <- compustat_segments %>% arrange(gvkey, year) %>% group_by(gvkey, year) %>% summarise(Countsegments = length(unique(snms))) %>% ungroup()
  compustat_segments <- compustat_segments %>% arrange(gvkey, year) %>% mutate(conglomerate = ifelse(Countsegments > 1, 1, 0))
  #######compustat#############
  comp <- comp %>% mutate(cusip6= substr(cusip,1,6))
  comp <- comp %>% rename(year = fyear)
  comp_ids <- comp %>% select(gvkey, conm, cusip, cusip6, year, sic, sich)
  comp_ids <- comp_ids %>% mutate(cusip_country = "US")
  #####SETWD######
  source(file.path(wd,"gai","boardex","functions","expandpanalbaordex.R"))
  source(file.path(wd,"gai","boardex","functions","split_index.R"))
  source(file.path(wd,"gai","boardex","functions","split_rolename.R"))
  source(file.path(wd,"gai","boardex","functions","split_sector.R"))
  source(file.path(~,"gai","boardex","functions","gai_industries_conglomerate.R"))
  source(file.path(~,"gai","boardex","functions","gai.R"))
  ##############
  list.of.sequences <- list()
  steps = 100
  jmp = round(nrow(boardex)/steps)
  for(i in 1:(steps-1)){
    list.of.sequences[[i]] = seq(1+(i-1)*jmp,i*jmp)
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
df <- df %>% mutate(cusip9 = substr(isin,3,11))
df <- df %>% mutate(cusip6= substr(isin,3,8))
df <- df %>% mutate(cusip_country = substr(isin,1,2))
df <- df %>% mutate(companyid = as.numeric(companyid))
df <- df %>% mutate(directorid = as.numeric(directorid))
print(dim(df))
`%!in%` <- Negate(`%in%`)
save.image(file.path(wd,"data/","boardex_gai.RData"))
rm(list= ls()[!(ls() %in% c('comp_ids','df','compustat_global_ids', 'compustat_segments'))])
sort(sapply(ls(), function(x) {object.size(get(x))})) 
compustat_global_ids2 <- compustat_global_ids %>% filter(isin %in% unique(df$isin))
save.image("problematic_data.RData")
gcs_upload(file = "problematic_data.RData",predefinedAcl = "bucketLevel",upload_type = "simple")
if(bk == T){
  break
}
sort(sapply(ls(), function(x) {object.size(get(x))})) 
compustat_global_ids <- compustat_global_ids %>% filter(isin %in% unique(df$isin))
gai_comp_us <- inner_join(comp_ids, df, by = c("cusip6", "year", "cusip_country"), multiple = "all")
gai_comp_int <- inner_join(compustat_global_ids, df, by = c("isin","year"), multiple = "all")
rm(list= ls()[!(ls() %in% c('gai_comp_int','gai_comp_us', 'compustat_segments'))])
isin_unique = c(unique(gai_comp_us$isin, gai_comp_int$isin))
df_isin_not_in_other <- df %>% filter(isin %!in% isin_unique)
dim(df_isin_not_in_other)
gai_comp <- list(gai_comp_us, gai_comp_int, df_isin_not_in_other) %>% rbindlist(fill = T)
#df <- df %>% group_by(directorid) %>% mutate(ceo = max(ceo_dummy)) %>% ungroup()
save.image(file.path(wd,"data/","boardex_gai_stage2.RData"))
df_factors <- gai_func(rd = F, dt = gai_comp)
upload_gcs_from_file(authfile=authfile,bucket = "corporate-finance-data",pth = "~",nm = "gai_factors.rds")





