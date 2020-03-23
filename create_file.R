create_file <- function(file_name, URL, records = 1000,  proportion = "N") {
  
  assign(deparse(substitute(file_name)), rep(0, records), envir = .GlobalEnv)
  
  lines <- readr::read_lines(URL)
  n_lines <- length(lines)
  
  if (proportion == "N"){index <- createDataPartition(1:n_lines, p = records/n_lines)}
  if (proportion != "N"){index <- createDataPartition(1:n_lines, p = records)}
  
  index <- unlist(index)
  lines <- lines[index]

  assign(deparse(substitute(file_name)), lines, envir = .GlobalEnv)
}