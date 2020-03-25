library(tidyverse)
library(knitr)
library(data.table)
library(quanteda)
library(methods)
library(utils)
library(LaF)
library(caret)
library(profvis)

set.seed(12349)

source("next_word.R")
source("good_turing.R")
source("model_prep.R")
source("load_data.R")
source("tune_model_size.R")

load_data()
tune_model_size(train, .02)

train_corp <- corpus(train)
test_corp <- corpus(test)

#data cleanup and manipulation 
combined_tok <- tokens(train_corp)
combined_tok <- tokens_tolower(combined_tok)
combined_tok <- tokens(combined_tok, remove_punct = TRUE, remove_numbers = TRUE)
combined_tok <- tokens_select(combined_tok, pattern = profanity, selection = "remove")
combined_tok <- tokens_select(combined_tok, pattern = all_english, selection = "keep")

rm(train_corp)
rm(profanity)
rm(all_english)

#build model

all_ps <- model_prep(combined_tok, 2)


results <- next_word("all the", all_ps)



