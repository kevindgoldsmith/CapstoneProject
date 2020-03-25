tune_model_size <- function(data, p){
  index <- unlist(createDataPartition(1:length(data), p = p))
  assign("train", data[index], envir = .GlobalEnv)
}