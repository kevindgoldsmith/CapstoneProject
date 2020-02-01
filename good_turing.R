good_turing <- function(words_dfm){
  
  max_freq <- max(textstat_frequency(words_dfm)$frequency)
  freqs <- textstat_frequency(words_dfm)$frequency
  
  freq_table <- matrix(nrow = max_freq, ncol = 2)
  
    for (i in 1 : max_freq) {
    freq_table[i, 1] <- log(i)
    freq_table[i, 2] <- length(which(freqs == i)) / length(freqs)
  } 
  
  nonzero_max <- min(which(freq_table[,2] == 0)) - 1
  
  freq_table <- freq_table[c(1:nonzero_max), ]
  freq_table[2] <- log(freq_table[2])
  freq_table <- as.data.frame(freq_table)
  names(freq_table) <- c("r", "N")
  out <- lm(N ~ r, freq_table)
  out$coefficients
  }