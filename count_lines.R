count_lines <- function(URL) {
  n_lines <- 1000
  increment <- 1000
  flag <- 0
  
  while (increment > 1){
    a = length(readr::read_lines(URL, skip = n_lines, n_max = 1))
    if(a == 0){
      if(flag == "up")
        {
        n_lines <- n_lines - increment / 2
        increment <- increment / 2
        }
      if(flag == "down"){n_lines <- n_lines - increment}
      flag <- "down"
    }
    if(a != 0){
      if (flag == "up") {
        n_lines <- n_lines + increment 
        increment <- increment * 2}
      if (flag == "down"){n_lines <- n_lines + increment / 2}
      flag <- "up"
    }
  }
  
  n_lines <- floor(n_lines)
  
  while(flag != "stop")
  {
    a = length(readr::read_lines(URL, skip = n_lines, n_max = 1))
    ifelse(a == 0, flag <- "stop", n_lines <- n_lines + 1)
    }
  
  return(n_lines)
}