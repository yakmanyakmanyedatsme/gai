gai_conglomerate_sectors <- function(dftemp, nm = NA,yr_min = NA){
  df_list <- list()
  i = 1
  year_list = unique(dftemp$year)[order(unique(dftemp$year))]
  if(!is.na(yr_min)){
    year_list = year_list[which(year_list >= yr_min)]
  }
  print(year_list)
  for(yr in year_list){
    dft <- dftemp %>% filter(year == yr)
    directorids <- unique(dft$directorid)
    dft <- dftemp %>% filter(year <= yr, directorid %in% directorids)
    dft1 <- dft %>% group_by(directorid) %>% summarise(max_segments = max(Countsegments, na.rm = T), year = yr) %>% ungroup()
    dft1 <- dft1 %>% group_by(directorid) %>% mutate(conglomerate_exp = ifelse(max_segments > 1 & !is.na(max_segments),1,0), year = yr) %>% ungroup()
    dft2 <- dft %>% filter(!is.na(sic))
    dft2 <- dft2 %>% group_by(directorid) %>% summarise(count_industries = length(unique(sic)), year = yr) %>% ungroup()
    df_list[[as.character(yr)]] <-  merge(dft1,dft2, by = c("directorid","year"))
    print(yr)
    saveRDS(df_list,"df_list_cong.rds")
  }
  tt <- df_list %>% bind_rows() %>% distinct()
  if(!is.na(nm)){
    write_dta(tt, nm)
    saveRDS(tt, file = nm)
  }
  return(tt)
}