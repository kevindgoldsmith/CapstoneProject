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
source("convert_to_data_frame.R")

load_data()
train_toks <- create_toks(train, .002)
test_toks <- create_toks(test, .000002)

rm(train)
rm(test)

test_df <- convert_to_data_frame(test_toks)

pred_model <- model_prep(train_toks, 2)

out <- score_model(pred_model, test_df)