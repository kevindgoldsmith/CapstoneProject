count_lines <- function(file_name, URL, records = 1000, increment = 10000) {
  
  flag = 0
  n_lines <<- 0
  while (flag != 1){
    a = length(readr::read_lines(URL, skip = n_lines, n_max = 2))
    if(a == 0) {flag = 1}
    n_lines <<- n_lines + increment
  }
  n_lines <<- n_lines - increment * 2
  flag = 0
  
  return(n_lines)
}