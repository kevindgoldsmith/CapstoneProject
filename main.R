library(tidyverse)
library(knitr)
library(data.table)
library(quanteda)
library(methods)
library(utils)
library(LaF)
library(caret)
library(profvis)
library(seplyr)


set.seed(12349)

source("next_word.R")
source("good_turing.R")
source("model_prep.R")
source("load_data.R")
source("create_toks.R")

load_data()
create_toks(train, .002)
create_toks(test, 1)


rm(train)


#build model

pred_model <- model_prep(train_tok, 2)


results <- next_word("all the", pred_model)



