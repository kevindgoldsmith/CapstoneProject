model_prep <- function(words_tok){
  combined_1grams <- dfm(tokens_ngrams(words_tok, 1))
  p_1s <- good_turing(combined_1grams)
  p_1s <- mutate(p_1s, ngram = 1)
  
  combined_2grams <- dfm(tokens_ngrams(words_tok, 2))
  p_2s <- good_turing(combined_2grams)
  p_2s <- mutate(p_2s, ngram = 2)
  
  combined_3grams <- dfm(tokens_ngrams(words_tok, 3))
  p_3s <- good_turing(combined_3grams)
  p_3s <- mutate(p_3s, ngram = 3)
  
  rbind(p_1s, p_2s, p_3s)
}