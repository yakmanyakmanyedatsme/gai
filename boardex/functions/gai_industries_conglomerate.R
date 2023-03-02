gai_conglomerate_sectors <- function(dftemp, nm = NA){
  #library(future.apply)
  #plan(multiprocess)
  #options(future.globals.maxSize= 29991289600)
  library(parallel)
  '%!in%' <- function(x,y)!('%in%'(x,y))
  dftemp <- dftemp %>% mutate(dir_com = paste(directorid,companyid,sep="_"))
  year_list = unique(dftemp$year)[order(unique(dftemp$year))]
  print(year_list)
  #################################Create_Cluster
  
  #######
  get_df_and_dir_com <- function(yr,dfttemp){
    print(yr)				
    dff <- dfttemp %>% filter(year == yr)
    dir_com_list <- unique(dff$dir_com)
    dfttemp <- dfttemp %>% filter(year <= yr)
    return(list(dir_com_list,dfttemp,yr))
  }
  #######
  data_list <- list()
  i = 1
  for(year in year_list){
    data_list[[i]] <- get_df_and_dir_com(yr = year, dfttemp = dftemp)
    i = i+1
  }
  #############################################
  ########Define Conglomerate industries#######
  #############################################
  get_cong_industries_for_yr <- function(obv_list){
    calc_cong_industries <- function(dir_com_val,dft,yr){
      library(dplyr)
      dir <- as.numeric(strsplit(dir_com_val, split = "_")[[1]][1])
      com <- as.numeric(strsplit(dir_com_val, split = "_")[[1]][2])
      dftt <- dft %>% filter(year < yr, dir_com != dir_com_val, directorid == dir)
      if(nrow(dftt) == 0){
        dftt <- as.data.frame(matrix(c(dir, com, 0, NA, 0, yr), ncol = 6))
        colnames(dftt) <- c("directorid", "companyid", "conglomerate_exp", "max_segments", "count_industries", "year")
        return(dftt)
      }else{
        dftt1 <- dftt %>% group_by(directorid) %>% summarise(max_segments = max(Countsegments, na.rm = T), year = yr) %>% ungroup()
        dftt1 <- dftt1 %>% group_by(directorid) %>% mutate(conglomerate_exp = ifelse(max_segments > 1 & !is.na(max_segments), 1, 0), year = yr, companyid = com) %>% ungroup()
        dftt2 <- dftt %>% filter(!is.na(sic))
        dftt2 <- dftt2 %>% group_by(directorid) %>% summarise(count_industries = length(unique(sic)), year = yr, companyid = com) %>% ungroup()
        return(merge(dftt1, dftt2, by = c("directorid", "companyid", "year"), all = T))
      }
    }
    dir_com_list = obv_list[[1]]
    dftt = obv_list[[2]]
    year = obv_list[[3]]
    print(year)
    return(lapply(dir_com_list,function(x) calc_cong_industries(dir_com_val = x, dft = dftt, yr = year)))
  }
  gc()
  num_cores <- detectCores()
  cl <- makeCluster(num_cores)
  dft_list <- parLapply(cl, data_list, get_cong_industries_for_yr)
  return(dft_list)
  print("finish calculating factors")
  dft_list <- parLapply(cl, dft_list, bind_rows)
  stopCluster(cl)
  print("finished binding sub list")
  tt <- dft_list %>% bind_rows() %>% distinct()
  tt <- tt %>% group_by(directorid, companyid) %>% mutate(count_industries = min(count_industries, na.rm = T),conglomerate_exp = min(conglomerate_exp, na.rm = T))
  if(!is.na(nm)){
    write_dta(tt, nm)
    saveRDS(tt, file = nm)
  }
  return(tt)
}
