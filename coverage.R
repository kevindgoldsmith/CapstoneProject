coverage <- function(dfm, percent){
  flag = 0
  partial = 0
  total = sum(textstat_frequency(dfm)$frequency)
  i = 1
  
  while (flag == 0){
    partial = partial + textstat_frequency(dfm)[i]$frequency
    if (partial >= total * (percent / 100)) {flag = 1}
    i = i + 1
  }
  i
}