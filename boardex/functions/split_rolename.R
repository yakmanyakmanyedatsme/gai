splt_role_name <- function(df, splt = "/"){
  df <- df %>% tidyr::separate_rows(rolename, sep = "/")
  return(df)
}