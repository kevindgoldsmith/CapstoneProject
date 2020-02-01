next_word <- function(firstword){
  possible_words <- kwic(combined_ngrams,
                         pattern = paste0("^",deparse(substitute(firstword)),"+_[^_]+$"),
                         valuetype = "regex")
  starts <- unname(sapply(possible_words[,5], str_locate, "_")[2,]) + 1
  ends <- unname(sapply(possible_words[,5], nchar))
  possible_words <- substr(possible_words[,5], starts, ends)
  selections <- as.data.frame(table(possible_words))
  selections$likelihood<- prop.table(selections$Freq)
  selections <- subset(selections, select = c(1,3))
  selections <- selections[order(-selections$likelihood),]
  rownames(selections) <- NULL
  head(selections, 10)
}