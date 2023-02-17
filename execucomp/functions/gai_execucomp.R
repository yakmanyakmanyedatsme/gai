gai_calc <- function(execucomp = NA,rd = F){
  i = 1
  library(tidyr)
  if(rd == T){
    execucomp <- read_dta("C:\\Users\\wfran\\Dropbox\\Data\\Compustat\\Execucomp\\dta\\Annual_Compensation_Execucomp.dta")
  }
  execucomp <- execucomp %>% tidyr::separate_rows(titleann, sep = ",")
  execucomp <- execucomp %>% tidyr::separate_rows(titleann, sep = "&")
  execucomp <- execucomp %>% mutate(titleann = tolower(trimws(titleann, which = c("both"))))
  df_list <- list()
  for(yr in unique(execucomp$year)){
    dftemp <- execucomp %>% filter(year <= yr)
    df_list[[i]] <- data.table(dftemp %>% group_by(execid) %>% summarise(
      Countpastfirms = length(unique(gvkey)), 
      Countpastpositions = length(unique(titleann)),
      Countpastindustries = length(unique(sic)),
      conglomerate_exp = max(conglomerate),
      year = yr+1))
    i = i+1
  }
  temp <- df_list %>% bind_rows()
  return(temp)
}
