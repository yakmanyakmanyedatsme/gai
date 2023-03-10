rm(list=ls())
library(dplyr)
df <- readRDS("C:\\Users\\wfran\\Dropbox\\Research Projects\\Executives\\Data\\gai_comp_seg2.rds")
gai <- readRDS("C:\\Users\\wfran\\Dropbox\\Research Projects\\Executives\\Data\\factors_us_public.rds")
boardex_expanded <- readRDS("C:\\Users\\wfran\\Dropbox\\Research Projects\\Executives\\Data\\boardex_expanded.rds")
boardex_expanded <- boardex_expanded %>% select(year, directorname, directorid, companyid, rolename) %>% distinct()
boardex_expanded <- boardex_expanded %>% mutate(year = as.numeric(year))
df <- df %>% select(gvkey,directorid, companyid, year, conm, companyname, cusip6, cusip9, sic, conm) %>% distinct()
df <- df %>% mutate(year = as.numeric(year))
gai <- gai %>% mutate(year = as.numeric(year))
df2 <- merge(df,boardex_expanded, by =c("directorid","companyid", "year"))
dft <-  merge(df2,gai,by = c("directorid" ,"companyid" ,"year"), all.y = T)
dft2 <-  merge(df2,gai,by = c("directorid" ,"companyid" ,"year"))
saveRDS(dft2,"C:\\Users\\wfran\\Dropbox\\Research Projects\\Executives\\Data\\gai_factors_and_ids.rds")