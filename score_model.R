score_model <- function(model, test){
  preds_1 <- map(test$input,
                 next_word,
                 probs = model,
                 verbose = "F",
                 n = 1)
  preds_3 <- map(test$input,
                 next_word,
                 probs = model,
                 verbose = "F",
                 n = 3)
  results_1 <- test$response %in% preds_1
  results_3 <- test$response %in% unlist(preds_3)
  out <- cbind(results_1, results_3)
  out
  }