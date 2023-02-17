rm(list=ls())
library("haven")
library("dplyr")
library("data.table")
`%!in%` <- Negate(`%in%`)
df <- fread("C:\\Users\\wfran\\Dropbox\\Research Projects\\Executives\\Data\\gai_factors_data_segments_sic.csv")
ids = unique(df$directorid)[1:5]
temp <- df %>% filter(directorid %in% ids)