#load packages
library(readr)
library(quanteda)
library(quanteda.textmodels)
library(readtext)
library(tidyverse)
library(data.table)


#dictionary for sentiment analysis
#https://www.rdocumentation.org/packages/quanteda/versions/2.1.2/topics/data_dictionary_LSD2015

#load data
Corp_HouseOfCommons_V2  <- readRDS("~/Desktop/tada-hoc/Corp_HouseOfCommons_V2.rds")
names(Corp_HouseOfCommons_V2)

#load and arragne dictionary
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

#dfm creates a document feature matrix
img_dfm <- dfm(speechcorp, dictionary = dict_img)

#see which words appeared most
textplot_wordcloud(img_dfm, max_words = 100, color = c('blue','yellow', 'green', 'orange'))

#convert data frame because it is easier to work with
img_df <- convert(img_dfm, to = "data.frame")
head(img_df)
