next_word <- function(string, probs, verbose = T, n = 3){
  ### determine number of words
  words <- str_count(string, pattern = "_") + 1
  
  ### save last word
  w <- word(string, - 1, sep = "_")
  
  ### save first word
  w_n1 <- word(string, -2, sep = "_")

  ### prob of observing last word
  p_w <- probs[which(probs$features == w), "true_est"]
  
  ### prob of observing both words together 
  p_w1 <- probs[which(probs$features == paste0(w_n1,"_", w)), 
    "true_est"]
  
  ### find all 3-grams starting with string 
  possible_words3 <- filter(probs, 
                            startsWith(features,
                                       paste0(w_n1, "_", w,"_")),
                            ngram == 3)
  possible_words3 <- possible_words3 %>% 
    mutate(last = word(features, -1, sep = "_"))
  
  ### final all 2-grams starting with last word 
  possible_words2 <- filter(probs, 
                            startsWith(features, paste0(w,"_")),
                            ngram == 2)
  possible_words2 <- possible_words2 %>% 
    mutate(last = word(features, -1, sep = "_"))
  possible_words2 <- anti_join(possible_words2, possible_words3,
                               by = c("last" = "last"))
  
  possible_words3 <- mutate(possible_words3, prob = pmin(gt_est / p_w1, 1))
  alpha3 <- 1 - sum(possible_words3$prob)
  
  possible_words2 <- mutate(possible_words2, prob = alpha3 * pmin(gt_est / p_w, 1))
  alpha2 <- 1 - sum(possible_words2$prob)
  
  lowest <- suppressWarnings(
    min(min(possible_words3$prob), min(possible_words2$prob)))
  
  if(lowest == Inf){lowest <- 0}
  
  possible_words1 <- filter(probs, 
                            gt_est > lowest,
                            ngram == 1)
  possible_words1 <- possible_words1 %>% 
    mutate(last = word(features, -1, sep = "_"))
  possible_words1 <- anti_join(possible_words1, possible_words2,
                               by = c("last" = "last"))
  possible_words1 <- anti_join(possible_words1, possible_words3,
                               by = c("last" = "last"))
  possible_words1 <- mutate(possible_words1, prob = alpha2 * gt_est)
  
  preds <- rbind(possible_words2, possible_words3, possible_words1)
  preds %<>% dplyr::select(c(last, ngram, prob))
  preds <- preds[order(preds$prob, decreasing = TRUE),]
  if(verbose == T) {
    if(nrow(preds) < n){
      new_row <- rep(NA, n)
      results <- rbind(preds, rep(new_row, n - nrow(preds)))}
    else {results <- head(preds, n)}
  }
  if(verbose != T) {
    if(nrow(preds) < n){results <- c(preds$last, rep(NA, n - nrow(preds)))}
    else {results <- head(preds$last, n)}
  }
  results
  }