split_list <- function(temp_lst, splt = 4500){
  library(future.apply)
  plan(multisession)
  options(future.globals.maxSize= 6891289600)
  main_lst <- list()
  len_lst = length(temp_lst)
  jmp = round(len_lst/splt)
  for(i in 1:(splt-1)){
    main_lst[[i]] <- seq(1+(i-1)*jmp,1+(i*jmp))
  }
  main_lst[[splt]] <- seq(1+(splt-1)*jmp,len_lst)
  j = 0
  main_lst <- future_lapply(main_lst, function(x,curr_lst = temp_lst){
    j <<- j+1
    print(j)
    return(curr_lst[x] %>% bind_rows())
  })
  return(main_lst)
}
