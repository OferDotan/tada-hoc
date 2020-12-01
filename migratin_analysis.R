#load packages
library(readr)
library(quanteda)
library(readtext)
library(tidyverse)


#dictionary for sentiment analysis
#https://www.rdocumentation.org/packages/quanteda/versions/2.1.2/topics/data_dictionary_LSD2015

#load and arragne dictionary
dict_img <- read_csv("immigration_500.csv")
dict_img <- dict_img %>% 
  subset(select = -c(`100.0`))
dict_img <- dict_img %>%
  pivot_wider(names_from= needs, values_from = needs)
rownames(dict_img ) <- c("immigration")
#convert dictionary df to a list
dict_list <- lapply(split(dict_img, row.names(dict_img)), unlist)
#turn to dictionary 
dict <- dictionary(dict_list)

#load data
Corp_HouseOfCommons_V2  <- readRDS("~/Desktop/FP_textasdata/Corp_HouseOfCommons_V2.rds")

#subset to speeches from 2006
speeches <- Corp_HouseOfCommons_V2 %>% 
  select(!c(iso3country, party.facts.id, parliament)) %>%
  filter(date>"2005-12-20")

#turn into corpus
speechcorp <- corpus(speeches)

#restrict to speeches with over 50 words
speechcorp <- corpus_subset(speechcorp, ntoken(speechcorp) > 50)

#summary
summary(speechcorp)

#dfm creates a document feature matrix
img_dfm <- dfm(speechcorp, dictionary = dict)

#convert data frame because it is easier to work with
#img_df <- convert(img_dfm, to = "data.frame")
#head(img_df)


