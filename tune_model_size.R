create_train_model <- function(data, p){
  index <- unlist(createDataPartition(1:length(data), p = p))
  train <- data[index]
  
  #data cleanup and manipulation 
  combined_tok <- tokens(train_corp)
  combined_tok <- tokens_tolower(combined_tok)
  combined_tok <- tokens(combined_tok, remove_punct = TRUE, remove_numbers = TRUE)
  combined_tok <- tokens_select(combined_tok, pattern = profanity, selection = "remove")
  combined_tok <- tokens_select(combined_tok, pattern = all_english, selection = "keep")
  
  
  assign("train_tok", combined_tok, envir = .GlobalEnv)
}