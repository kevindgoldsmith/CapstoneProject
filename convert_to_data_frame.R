convert_to_data_frame <- function(toks){
  three_gram <- tokens_ngrams(toks, 3)
  unlisted <- unlist(three_gram)
  vectorized <- as.vector(unlisted)
  results <- setNames(
    data.frame(
      matrix(ncol = 2, nrow = length(vectorized))
      ), 
    c("input", "response")
    )
  results[,"input"] <- word(vectorized, start = -3, end = -2, sep = "_")
  results[,"response"] <- word(vectorized, start = -1, sep = "_")
  results
}