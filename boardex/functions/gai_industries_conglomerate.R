gai_conglomerate_sectors <- function(dftemp){
  df_list <- list()
  i = 1
  for(yr in unique(dftemp$year)[order(unique(dftemp$year))]){
    dft <- dftemp %>% filter(year <= yr)
    dft1 <- dft %>% group_by(directorid) %>% summarise(max_segments = max(Countsegments, na.rm = T), year = yr) %>% ungroup()
    dft1 <- dft1 %>% group_by(directorid) %>% mutate(conglomerate_exp = ifelse(max_segments > 1 & !is.na(max_segments),1,0), year = yr) %>% ungroup()
    dft2 <- dft %>% filter(!is.na(sic))
    dft2 <- dft2 %>% group_by(directorid) %>% summarise(count_industries = length(unique(sic)), year = yr) %>% ungroup()
    df_list[[i]] <-  merge(dft1,dft2, by = c("directorid","year"))
      if(yr == 1995){
        View(dft2)
        View(dft1)
      }
    i = i+1
  }
  tt <- df_list %>% bind_rows()
  if(!is.na(nm)){
    write_dta(tt, nm)
    saveRDS(tt, file = nm)
  }
  return(tt)
}

tt = gai_conglomerate_sectors(dftemp = temp)
View(tt)