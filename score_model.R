score_model <- function(model, test, n){
  start_time <- Sys.time()
  preds <- map(test$input,
                 next_word,
                 probs = model,
                 verbose = F,
                 n = n)
  total_time <- Sys.time() - start_time
  average_time <- total_time / nrow(test)
  
  preds <- data.frame(matrix(unlist(preds), 
                             nrow = length(preds), 
                             byrow=T),
                      stringsAsFactors=FALSE)

  first_match <- rep(NA, nrow(preds))
  any_match <- rep(NA, nrow(preds))
  
  for (i in seq_len(nrow(test))){
    first_match[i] <- test$response[i] %in% preds[i,1]
  }
  
  for (i in seq_len(nrow(test))){
    any_match[i] <- test$response[i] %in% preds[i,1:n]
  }
  
  first_match_percent <- sum(first_match == TRUE)/length(first_match)
  any_match_percent <- sum(any_match == TRUE)/length(any_match)
  c("first_match" = first_match_percent, 
    "any_match" = any_match_percent,
    "average_time" = average_time)
}