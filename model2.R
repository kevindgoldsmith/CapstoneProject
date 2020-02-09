model2 <- function(string, probs){
  words <- str_count(string, pattern = "_")
  w <- word(string, - 1)
  w_n1 <- word(string, -2)

  p_w <- probs[which(probs$features == w), 2]
  p_w1 <- probs[which(probs$features == paste0(
    w_n1,"_", w
  )), 2]
  
  possible_words2 <- filter(probs, 
                           startsWith(features, paste0(w,"_")),
                           ngram == 2)
  
  possible_words3 <- filter(probs, 
                            startsWith(features,
                                       paste0(w_n1, "_", w,"_")),
                            ngram == 3)
  
  possible_words3 <- mutate(possible_words3, prob = p_ind / p_w1)
  alpha3 <- 1 - sum(possible_words3$prob)
  
  possible_words2 <- mutate(possible_words2, prob = alpha3 * p_ind / p_w)
  alpha2 <- 1 - sum(possible_words2$prob)
  
  lowest <- suppressWarnings(
    min(min(possible_words3$prob), min(possible_words2$prob)))
  lowest < if(lowest == Inf){0}
  
  possible_words1 <- filter(probs, 
                            p_ind > lowest,
                            ngram == 1)
  possible_words1 <- mutate(possible_words1, prob = alpha2 * p_ind)
  
  preds <- rbind(possible_words2, possible_words3, possible_words1)
  preds$next_word <- word(preds$features, -1, sep = "_")
  preds <- select(preds, c(next_word, ngram, prob))
  preds <- preds[order(preds$prob, decreasing = TRUE),]
  preds
  }