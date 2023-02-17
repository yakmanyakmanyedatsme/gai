splt_sectors <- function(df){
  df <- df %>% tidyr::separate_rows(secotrs, sep = "&")
  df <- df %>% tidyr::separate_rows(secotrs, sep = "and")
  return(df)
}