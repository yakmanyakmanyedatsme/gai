source(file.path(wd,"functions","gai_industries_conglomerate.R"))
temp2 <- merge(temp,gai_conglomerate_sectors(dftemp = temp), by = c("directorid","year"))
temp2 <- temp2 %>% select(c(gvkey, directorname, directorid, companyid, year, rolename, minceoyear,
                            ceo_dummy, Countpastfirms, Countpastpositions, Countsegments, max_segments,
                            conglomerate_exp, count_industries)) %>% arrange(directorid, year)