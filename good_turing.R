good_turing <- function(words_dfm){
## this function applies Good Turing discounting to output a matrix of 
  #smoothed likelihood predictions by words 
  
  #data prep
  features <- textstat_frequency(words_dfm)$feature
  freqs <- textstat_frequency(words_dfm)$frequency
  out <- cbind.data.frame(features, freqs)
  max_freq <- max(textstat_frequency(words_dfm)$frequency)
  freq_table_full <- setNames(data.frame(matrix(ncol = 3, nrow = max_freq)), 
                              c("r", "N_r", "prob"))
    
  #calculate N, r, and N_r 
  N <- sum(freqs)
  
  freq_table_full[,"r"] <- 1:max_freq
  freq_table_full[, "N_r"] <- match(row_number(freq_table_full$r), freqs)
  freq_table_full$prob = freq_table_full$r / N 
  
  #limit table to highest r which N_r > 0
  nonzero_max <- suppressWarnings(
    min(which(freq_table_full[,2] == 0)) - 1)
  if(nonzero_max == Inf) {nonzero_max = max_freq}
  freq_table <- freq_table_full[c(1:nonzero_max), ]
  
  #calculate log smoothed approximation of emperical data
  lp <- glm(N_r ~ log(r), freq_table, family = "poisson")
  new_data <- data.frame(r = 1:(max_freq + 1))
  preds <- exp(predict(lp, newdata = new_data))

  #use emperical data to N = 10 or nonzero_max, smoothed estimates after
  emp_max <- min(10, nonzero_max)
  gt_S <- rep(NULL, max_freq)
  gt_S[1:emp_max] <- freq_table[1:emp_max, "N_r"]
  gt_S[(emp_max + 1):(max_freq + 1)] <- preds[(emp_max + 1):(max_freq + 1)]
  
  #calculate probs by frequency
  gt_p <- setNames(data.frame(matrix(ncol = 3, nrow = max_freq + 1)), 
                   c("r", "p", "p_ind"))
  gt_p[,"r"] <- 1:(max_freq  + 1) - 1 
  gt_p[,"p"] <- (row_number(gt_p$r) * gt_S) / N

  
  #rebalance probs to sum to 1 
  rebal <- sum(gt_p$p)
  gt_p$p <- gt_p$p / rebal
  
  #calculate probs for individual words
  gt_p[1, "p_ind"] <- 0
  for (i in 2 : (max_freq + 1)) {
    gt_p[i, "p_ind"] <- gt_p[i, "p"] / gt_S[i - 1]
  }
  
  ###need to assign probabilities to each word
  out <- inner_join(out, gt_p, by = c("freqs" = "r"))
  out <- inner_join(out, freq_table_full, by = c("freqs" = "r"))
  out$gt_est <- out$p_ind
  out$true_est <- out$prob
  out <- subset(out, select = c(features, gt_est, true_est))
  out$features <- as.character(out$features)
  
  #cleanup 
  rm(freq_table)
  rm(gt_S)
  rm(gt_p)
  out
  }