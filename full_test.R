full_test <- function(train_data, test_df, p, min_words, stem = F, n_score = 3){
  train_toks <- create_toks(train_data, p, stem)
  pred_model <- model_prep(train_toks, min_words)
  out <- score_model(pred_model, test_df, n_score)
  model_size <- print(object.size(pred_model), units = "Mb")
  out <- c(out, "model_size" = model_size)
  out
}