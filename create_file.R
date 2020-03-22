create_file <- function(file_name, URL, records = 1000,  proportion = "N") {
  
  assign(deparse(substitute(file_name)), rep(0, records), envir = .GlobalEnv)
  helper <- rep(0, records)
  n_lines <- count_lines(URL)
  
  if (proportion == "N"){index <- createDataPartition(1:n_lines, p = records/n_lines)}
  if (proportion != "N"){index <- createDataPartition(1:n_lines, p = records)}
  index <- unlist(index)
  items <- length(index)
    for (i in 1:items){
      helper[i] <- readr::read_lines(URL, skip = index[[i]],n_max = 1)
    }
  
  assign(deparse(substitute(file_name)), helper, envir = .GlobalEnv)
}