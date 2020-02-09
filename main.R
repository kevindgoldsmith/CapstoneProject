library(knitr)
library(readr)
library(data.table)
library(tm)
library(quanteda)
library(readtext)
library(methods)
library(utils)
library(LaF)
library(ggplot2)
library(stringr)
library(dplyr)
library(rbenchmark)
set.seed(12345)

source("create_file.R")
source("next_word.R")
source("good_turing.R")
source("model_prep.R")

benchmark(
#import external filesl
download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", 
              destfile = "rawdata.zip", method = "curl"),
unzip("rawdata.zip"),

download.file("https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en", 
              destfile = "profanity.txt", method = "curl"),

download.file("https://raw.githubusercontent.com/dwyl/english-words/master/words.txt", 
              destfile = "english.txt", method = "curl"),

profanity <- readLines("profanity.txt"),
all_english <- readLines("english.txt"),

#Create combined corpus
create_file(US.blogs_corp, "~/CapstoneProject/final/en_US/en_US.blogs.txt", 50000),
create_file(US.twit_corp, "~/CapstoneProject/final/en_US/en_US.twitter.txt", 50000),
create_file(US.news_corp, "~/CapstoneProject/final/en_US/en_US.news.txt", 50000),

US.blogs_corp <- corpus(US.blogs_corp),
US.news_corp <- corpus(US.news_corp),
US.twit_corp <- corpus(US.twit_corp),
combined_corp <- US.blogs_corp + US.news_corp + US.twit_corp,

#data cleanup and manipulation 
combined_tok <- tokens(combined_corp), 
combined_tok <- tokens_tolower(combined_tok),
combined_tok <- tokens(combined_tok, remove_punct = TRUE, remove_numbers = TRUE),
combined_tok <- tokens_select(combined_tok, pattern = profanity, selection = "remove"),
combined_tok <- tokens_select(combined_tok, pattern = all_english, selection = "keep"),
combined_dfm <- dfm(combined_tok),
rm(profanity),
rm(all_english),

#build model
all_ps <- model_prep(combined_dfm),
model2("all the", all_ps),
replications = 1)


