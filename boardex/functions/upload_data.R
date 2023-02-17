upload_gcs_from_file <- function(authfile, bucket, eml, pth, nm){
  Sys.setenv("GCS_AUTH_FILE" = authfile)
  Sys.setenv("GCS_DEFAULT_BUCKET"= bucket)
  gcs_auth(email = eml)
  gcs_global_bucket(bucket)
  return(gcs_upload(file = file.path(pth,nm),predefinedAcl = "bucketLevel",upload_type = "simple"))
}
upload_gcs_from_ram <- function(authfile, bucket, eml, dt, pth, nm){
  Sys.setenv("GCS_AUTH_FILE" = authfile)
  Sys.setenv("GCS_DEFAULT_BUCKET"= bucket)
  gcs_auth(email = eml)
  gcs_global_bucket(bucket)
  return(gcs_save(dt, file.path(pth,nm)))
}
temp = upload_gcs_from_ram(authfile="~/gai/fluted-century-372700-7fbd134783e5.json",
                            bucket = "corporate-finance-data",pth = "~",
                            dt = dt, nm = "gai_factors.rds", eml="wakaflocka12211@gmail.com")
temp = upload_gcs_from_file(authfile="~/gai/fluted-century-372700-7fbd134783e5.json",
                            bucket = "corporate-finance-data",pth = "~",
                            nm = "gai_factors.rds", eml="wakaflocka12211@gmail.com")