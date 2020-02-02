good_turing <- function(words_dfm){
## this function applies Good Turing discounting to output a matrix of 
  #smoothed likelihood predictions by words 
  
  #data prep
  freqs <- textstat_frequency(words_dfm)$frequency
  max_freq <- max(textstat_frequency(words_dfm)$frequency)
  freq_table <- data.frame(r = integer(), N_r = double())
  
  #calculate N, r, and N_r 
  N <- sum(freqs)
    for (i in 1 : max_freq) {
    freq_table[i, "r"] <- i
    freq_table[i, "N_r"] <- length(which(freqs == i))
  } 
  
  #limit table to highest r which N_r > 0
  nonzero_max <- min(which(freq_table[,2] == 0)) - 1
  freq_table <- freq_table[c(1:nonzero_max), ]
  
  #calculate log smoothed approximation of emperical data
  lp <- glm(N_r ~ log(r), freq_table, family = "poisson")
  new_data <- data.frame(r = 1:(max_freq + 1))
  preds <- exp(predict(lp, newdata = new_data))

  #use emperical data to N = 10, smoothed estimates after 
  gt_S <- rep(NULL, max_freq)
  gt_S[1:10] <- freq_table[1:10, "N_r"]
  gt_S[11:(max_freq + 1)] <- preds[11:(max_freq + 1)]
  
  #calculate probs by frequency
  gt_p <- data.frame(r = integer(), p = double(), p_ind = double())
  for (i in 1 : (max_freq + 1)) {
    gt_p[i, "r"] <- i - 1
    gt_p[i, "p"] <- (i * gt_S[i]) / N
  }
  
  #rebalance probs to sum to 1 
  rebal <- sum(gt_p$p)
  gt_p$p <- gt_p$p / rebal
  
  #calculate props for individual words
  gt_p[1, "p_ind"] <- 0
  for (i in 2 : (max_freq + 1)) {
    gt_p[i, "p_ind"] <- gt_p[i, "p"] / gt_S[i - 1]
  }
    
  gt_p <<- gt_p
  }