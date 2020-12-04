#load packages
library(readr)
library(quanteda)
library(quanteda.textmodels)
library(readtext)
library(tidyverse)
library(data.table)
library(stm)

#dictionary for sentiment analysis
#https://www.rdocumentation.org/packages/quanteda/versions/2.1.2/topics/data_dictionary_LSD2015

#load data
Corp_HouseOfCommons_V2  <- readRDS("~/Desktop/tada-hoc/Corp_HouseOfCommons_V2.rds")
names(Corp_HouseOfCommons_V2)

#load and arragne dictionary
#the dictionary is from: https://blog.koheiw.net/?p=79
dict_img <- read_csv("immigration_500.csv")
dict_img <- dict_img %>% 
  subset(select = -c(`100.0`))
#pivot wider
dict_img <- dict_img %>%
  pivot_wider(names_from= needs, values_from = needs, values_fn = list)
rownames(dict_img ) <- c("immigration")
#convert dictionary df to a list
dict_list <- lapply(dict_img, unlist)

#turn to dictionary 
dict_img <- dictionary(dict_list)

#subset to speeches from 2010 (Justification:Tory manifesto)
speeches <- Corp_HouseOfCommons_V2 %>% 
  select(!c(iso3country, party.facts.id, parliament)) %>%
  filter(date>"2009-12-20")

#turn into corpus
speechcorp <- corpus(speeches)

#summary
summary(speechcorp)

#restrict to speeches with over 50 words
#clean_corp <- corpus_subset(speechcorp, ntoken(speechcorp) > 100)

#dfm creates a document feature matrix with immigration policy words
img_dfm <- dfm(speechcorp, 
               remove_punct = TRUE,
               remove = stopwords(),
               remove_symbols = TRUE,
               remove_separators = TRUE,
               split_hyphens = TRUE,
               remove_numbers = TRUE,
               dictionary = dict_img)
#create dfm with the dictionary with sentiments dictionary
####but in order to use it we need to add party variable or something
sentiment_dfm <- dfm(speechcorp, 
                     remove_punct = TRUE,
                     remove = stopwords(),
                     remove_symbols = TRUE,
                     remove_separators = TRUE,
                     split_hyphens = TRUE,
                     remove_numbers = TRUE,
                     dictionary = data_dictionary_LSD2015)
#see which words appeared most
textplot_wordcloud(img_dfm, max_words = 90, color = c('blue','purple','orange'))

#convert data frame because it is easier to work with
img_df <- convert(img_dfm, to = "data.frame")
head(img_df)

##Topic modeling 
# fit a simple model
mod <- stm(img_dfm, K = 6, seed = 12345)
labelTopics(mod)
plot(mod, type = "labels", labeltype = "prob") # or frex, lift, score


