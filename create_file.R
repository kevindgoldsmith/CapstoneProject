create_file <- function(file_name, URL, records = 1000, increment = 10000) {
  
  assign(deparse(substitute(file_name)), rep(0, records), envir = .GlobalEnv)
  helper <- rep(0, records)
  
  flag = 0
  n_lines <<- 0
  while (flag != 1){
    a = length(readr::read_lines(URL, skip = n_lines, n_max = 2))
    if(a == 0) {flag = 1}
    n_lines <<- n_lines + increment
  }
  n_lines <<- n_lines - increment * 2
  flag = 0
  for (i in 1:records){
    helper[i] <- readr::read_lines(URL, skip =
                                     round(runif(1, min = 1, max = n_lines)),
                                   n_max = 1)
  }
  
  assign(deparse(substitute(file_name)), helper, envir = .GlobalEnv)
}