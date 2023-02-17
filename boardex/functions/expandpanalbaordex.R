result <- list()
expandemployment <- function(temp = tt, dt1 = "datestartrole", dt2="dateendrole"){
  condition <- lapply(temp$datestartrole, is.na)
  for(i in (1:nrow(temp))){
    if(condition[[i]]){
      print(i)
      next
    }else{
      if(!is.na(temp[i,c(dt2)][[1]])){
        dt_end = format(as.Date(temp[i,c(dt2)][[1]], format = "%Y-%m-%d"), "%Y")
      }else{
        dt_end = format(as.Date("2021-12-01", format = "%Y-%m-%d"), "%Y")
      }
      
      dt_start = format(as.Date(temp[i,c(dt1)][[1]], format = "%Y-%m-%d"), "%Y")
      if(dt_start < dt_end){
        date_vec = seq(as.numeric(dt_start), as.numeric(dt_end))
      }else{
        date_vec = seq(as.numeric(dt_end), as.numeric(dt_start))
      }
      len_vec = length(date_vec)
      date_mat <- as.data.frame(matrix(c(date_vec, 
                                         rep(temp[i,c("directorid")][[1]], len_vec), 
                                         rep(temp[i,c("companyid")][[1]], len_vec), 
                                         rep(temp[i,c("directorname")][[1]], len_vec), 
                                         rep(temp[i,c("companyname")][[1]], len_vec), 
                                         rep(temp[i,c("rolename")][[1]], len_vec),
                                         rep(temp[i,c("isin")][[1]], len_vec),
                                         rep(temp[i,c("hocountryname")][[1]], len_vec),
                                         rep(temp[i,c("orgtype")][[1]], len_vec)),
                                       ncol=9))
      colnames(date_mat) <- c("year", "directorid", "companyid", "directorname", "companyname", "rolename", "isin", "hocountryname", "orgtype")
      result[[i]] <<- date_mat
      j <<- i
      if(i %% 100 == 0){
        print(i)
      }else if(i %% 20000 == 0){
        saveRDS(result, file="resultlist.RData")
      }
    }
  }
  return(result)
}