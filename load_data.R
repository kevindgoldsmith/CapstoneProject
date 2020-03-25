load_data <- function(){
  #import external files
  if(!file.exists("rawdata.zip")){
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                  destfile = "rawdata.zip", method = "curl")
    unzip("rawdata.zip")
  }
  
  
  if(!file.exists("profanity.txt")){
    download.file("https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en",
                  destfile = "profanity.txt", method = "curl")
  }
  
  if(!file.exists("english.txt")){
    download.file("https://raw.githubusercontent.com/dwyl/english-words/master/words.txt",
                  destfile = "english.txt", method = "curl")
  }
  
  file.remove("rawdata.zip")
  
  
  profanity <- readLines("profanity.txt")
  all_english <- readLines("english.txt")
  
  #Create combined corpus
  US.blogs_corp <- readr::read_lines("~/CapstoneProject/final/en_US/en_US.blogs.txt")
  US.twit_corp <- readr::read_lines("~/CapstoneProject/final/en_US/en_US.twitter.txt")
  US.news_corp <- readr::read_lines("~/CapstoneProject/final/en_US/en_US.news.txt")
  unlink("~/CapstoneProject/final", recursive = TRUE)
  
  combined <- c(US.blogs_corp, US.twit_corp, US.news_corp)
  
  index <- unlist(createDataPartition(1:length(combined), p = .8))

  assign("train", combined[index], envir = .GlobalEnv)
  assign("test", combined[-index], envir = .GlobalEnv)
}