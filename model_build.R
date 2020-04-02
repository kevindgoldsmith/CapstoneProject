model_build <- function(model_prep, n = 3){
  input <- model_prep %>% filter(ngram %in% 1:2) %>% select(features)
  preds <- map(input$features, 
                 next_word, probs = model_prep, verbose = "F", n = n)
  
  results <- cbind("inputs" = input$features, "preds" = preds)
  results
}