gai_func <- function(rd = F, dta = F, dt = NA, nm = "gai_factors.rds"){
  library(haven)
  library(dplyr)
  library(data.table)
  gai_components <- function(dftemp){
    df_list <- list()
    i = 1
    for(yr in unique(dftemp$year)){
      dft <- dftemp %>% filter(year == yr)
      directorids <- unique(dft$directorid)
      dft <- dftemp %>% filter(year <= yr, directorid %in% directorids)
      df_list[[i]] <- data.table(dft %>% group_by(directorid) %>% summarise(Countpastfirms = length(unique(companyid)), 
                                                                            Countpastpositions = length(unique(rolename)),
                                                                            year = yr) %>% ungroup())
      saveRDS(df_list,"df_list_exp.rds")
      i = i+1
      print(yr)
    }
    temp <- df_list %>% bind_rows()
    return(data.table(temp))
  }
  gai_ceo_subsample <- function(dftemp){
    '%!in%' <- function(x,y)!('%in%'(x,y))
    indx1 = unique(dftemp$rolename[grep(paste(c("Chief Executive", "CEO", "ceo"), collapse="|"),dftemp$rolename)])
    indx2 = c("Assistant Divisional Chief Executive", "Regional Chief Executive", "Chief Executive Janitor", "Division")
    temp <- dftemp[which(dftemp$rolename %in% indx1 & dftemp$rolename %!in% indx2),]
    temp <- temp %>% group_by(directorid) %>% summarise(minceoyear = min(year))
    dftemp <- merge(dftemp, temp, by = c("directorid"), all.x = T)
    dftemp <- dftemp %>% mutate(ceo_exp_dummy = ifelse(year >= minceoyear & !is.na(minceoyear), 1, 0))  %>% select(directorid, companyid, year, minceoyear, ceo_exp_dummy)
    dftemp <- dftemp %>% filter(!duplicated(dftemp))
    return(data.table(dftemp))
  }
  # test if there is at least one argument: if not, return an error
  if(rd == F){
    print("Read in data from RAM")
  }else{
    args = commandArgs(trailingOnly=TRUE)
    if(length(args)==0){
      stop("At least one argument must be supplied (input file).n", call.=FALSE)
    }else{
      fls = args[1]
      if(dta == T){
        dt <- read_dta(fls)
      }else{
        # Restore the objec
        readRDS(file = fls)
      }
    }
  }
  tt1 <- gai_ceo_subsample(dt) %>% arrange(directorid, year) %>% select(directorid, companyid, year, minceoyear, ceo_exp_dummy)
  print("finish tt1")
  tt2 <- gai_components(dt) %>% arrange(directorid, year)
  tt3 <- gai_conglomerate_sectors(dt) %>% arrange(directorid, year) 
  print("finish tt2")
  tt <- merge(tt2,tt1,by= c("directorid", "year"))
  tt <- merge(tt,tt3, by= c("directorid", "year"))
  tt <- as.data.table(tt)
  #write_dta(tt, nm)
  saveRDS(tt, file = nm)
  return(tt)
}

#tt1 <- tt1 %>% rowwise() %>% mutate(dyear = paste0(directorid, year, collapse = "_"))
#tt2 <- tt2 %>% rowwise() %>% mutate(dyear = paste0(directorid, year, collapse = "_"))
