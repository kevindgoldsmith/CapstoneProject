next_word <- function(string, probs, verbose = T, n = 3){
  ### determine number of words
  words <- str_count(string, pattern = "_") + 1
  
  ###save last word and prob 
  w <- word(string, - 1, sep = "_")
  p_w <- probs[which(probs$features == w), "true_est"]
  
  ##if more than 1 word, 3 gram logic 
  if(words > 1){
         w_n1 <- word(string, -2, sep = "_")
         p_w1 <- probs[which(probs$features == paste0(w_n1,"_", w)), "true_est"]
         possible_words3 <- filter(probs, 
                                   startsWith(features,
                                              paste0(w_n1, "_", w,"_")),
                                   ngram == 3)
         possible_words3 <- mutate(possible_words3, prob = pmin(gt_est / p_w1, 1))
         alpha3 <- 1 - sum(possible_words3$prob)
  }
  
  ### 2 gram logic  
  possible_words2 <- filter(probs, 
                            startsWith(features, paste0(w,"_")),
                            ngram == 2)
  ## antijoin 2 grams and 3 grams if words > 1
  if(words > 1){
    possible_words2 <- anti_join(possible_words2, possible_words3,
                                 by = c("last" = "last"))
    possible_words2 <- mutate(possible_words2, prob = alpha3 * pmin(gt_est / p_w, 1))
  }
  else
  {possible_words2 <- mutate(possible_words2, prob = pmin(gt_est / p_w, 1))}
  
  
  alpha2 <- 1 - sum(possible_words2$prob)
  
  possible_words1 <- probs %>% filter(ngram == 1) %>% top_n(10, gt_est)
  possible_words1 <- anti_join(possible_words1, possible_words2,
                               by = c("last" = "last"))
  if(words > 1){
    possible_words1 <- anti_join(possible_words1, possible_words3,
                                 by = c("last" = "last"))
  }
  possible_words1 <- mutate(possible_words1, prob = alpha2 * gt_est)
  
  if(words > 1){
    possible_words3 %<>% top_n(10, prob)
  }
  possible_words2 %<>% top_n(10, prob)
  possible_words2 %<>% top_n(10, prob)
  if(words > 1){
    preds <- rbind(possible_words2, possible_words3, possible_words1)
  }
  else{
    preds <- rbind(possible_words2, possible_words1)
  }
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