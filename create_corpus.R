create_toks <- function(data_in, p){
  
  name_in <- enexpr(data_in)
  name <- paste0(name_in, "_toks")
  
  if(p != 1){
    index <- unlist(createDataPartition(1:length(data_in), p = p))
    data_in <- data_in[index]
  }
  
  
  profanity <- readLines("profanity.txt")
  all_english <- readLines("english.txt")
  
  #data_incleanup and manipulation 
  data_corp <- corpus(data_in)
  data_corp <- tokens(data_corp)
  data_corp <- tokens_tolower(data_corp)
  data_corp <- tokens(data_corp, remove_punct = TRUE, remove_numbers = TRUE)
  data_corp <- tokens_select(data_corp, pattern = profanity, selection = "remove")
  data_corp <- tokens_select(data_corp, pattern = all_english, selection = "keep")

  rm(profanity)
  rm(all_english)
  
  assign(name, data_corp, envir = .GlobalEnv)
}