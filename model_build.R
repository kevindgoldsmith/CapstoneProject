model_build <- function(probs, n = 3){
  start <- Sys.time()
  last_time <- start
  one_gram <- probs %>% filter(ngram == 1) %>% top_frac(.75, true_est)
  two_gram <- probs %>% filter(ngram == 2) %>% top_frac(.75, true_est)
  input <- rbind(one_gram, two_gram)
  input %<>% dplyr::select(features)
  input <- as.vector(input$features)
  rm(one_gram)
  rm(two_gram)
  out <- setNames(data.frame(matrix(ncol = 3, nrow = length(input))), 
                             c("pred1", "pred2", "pred3"))
  chunks <- split(input, ceiling(seq_along(input)/(length(input)/20)))
  row_num = 1
  numCores <- detectCores()
  registerDoParallel(numCores)
  for (i in 1:length(chunks)){

      preds <- mclapply(chunks[[i]],
                next_word,
                probs = probs,
                verbose = "F",
                n = n,
                mc.cores = numCores,
                mc.preschedule = TRUE)
    preds <- as.data.frame(do.call(rbind, preds), stringsAsFactors = FALSE)
    names(preds) <- c("pred1", "pred2", "pred3")
  out[row_num:(row_num + nrow(preds) - 1), ] <- preds
  row_num <- row_num + nrow(preds) 
  rm(preds)
  now_time <- Sys.time()
  total_time <- difftime(now_time, start, units = "mins")
  this_round <- difftime(now_time, last_time, units = "mins")
  last_time <- now_time
  cat(i, " of ", length(chunks), "completed\n")
  cat(this_round, "m round time\n")
  cat(total_time / i, "m average time\n")
  cat(total_time, "m total time\n\n")
  }
  
  stopImplicitCluster()
  results <- cbind(input, out)
  results
}