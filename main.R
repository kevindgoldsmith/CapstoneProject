library(tidyverse)
library(knitr)
library(readr)
library(data.table)
library(quanteda)
library(methods)
library(utils)
library(LaF)
library(ggplot2)
library(stringr)
library(dplyr)
library(caret)
library(profvis)
set.seed(12348)

source("create_file.R")
source("next_word.R")
source("good_turing.R")
source("model_prep.R")

#import external filesl
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
create_file(US.blogs_corp, "~/CapstoneProject/final/en_US/en_US.blogs.txt", 
            records = .5, proportion = "Y")
create_file(US.twit_corp, "~/CapstoneProject/final/en_US/en_US.twitter.txt", 
            records = .5, proportion = "Y")
create_file(US.news_corp, "~/CapstoneProject/final/en_US/en_US.news.txt", 
            records = .5, proportion = "Y")

unlink("~/CapstoneProject/final", recursive = TRUE)

US.blogs_corp <- corpus(US.blogs_corp)
blogs_names <- rep("NA", ndoc(US.blogs_corp))
blogs_names <- paste0("blogs_",row_number(blogs_names))
docnames(US.blogs_corp) <- blogs_names

US.news_corp <- corpus(US.news_corp)
news_names <- rep("NA", ndoc(US.news_corp))
news_names <- paste0("news_",row_number(news_names))
docnames(US.news_corp) <- news_names
  
US.twit_corp <- corpus(US.twit_corp)
twit_names <- rep("NA", ndoc(US.twit_corp))
twit_names <- paste0("twit_",row_number(twit_names))
docnames(US.twit_corp) <- twit_names

combined_corp <- US.blogs_corp + US.news_corp + US.twit_corp
rm(US.blogs_corp)
rm(US.news_corp)
rm(US.twit_corp)
rm(blogs_names)
rm(news_names)
rm(twit_names)

#data cleanup and manipulation 
combined_tok <- tokens(combined_corp)
rm(combined_corp)
combined_tok <- tokens_tolower(combined_tok)
combined_tok <- tokens(combined_tok, remove_punct = TRUE, remove_numbers = TRUE)
combined_tok <- tokens_select(combined_tok, pattern = profanity, selection = "remove")
combined_tok <- tokens_select(combined_tok, pattern = all_english, selection = "keep")

rm(profanity)
rm(all_english)

#build model

all_ps <- model_prep(combined_tok)


profvis(results <- next_word("all the", all_ps))



