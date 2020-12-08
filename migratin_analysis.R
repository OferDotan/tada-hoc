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

# Amir: This one might be more relevant to us: http://www.snsoroka.com/data-lexicoder/ 
# although it is actually designed for use on political speech (often related to migration) [but] on social meda...
# for reference, see https://www.tandfonline.com/doi/full/10.1080/1369183X.2019.1665990 (section 3.2. Measuring the sentiment of migration discourse on Facebook)
# for another description of what the Lexicoder does, see https://ijoc.org/index.php/ijoc/article/view/6273/1946, page 975

#load data
Corp_HouseOfCommons_V2  <- readRDS("Corp_HouseOfCommons_V2.rds")
names(Corp_HouseOfCommons_V2)

# some examples of agendas
head(unique(Corp_HouseOfCommons_V2$agenda))
#number of agendas
length(unique(Corp_HouseOfCommons_V2$agenda))
length(Corp_HouseOfCommons_V2$agenda)
Corp_HouseOfCommons_V2$agenda["migration"]
?contains()

#load and arragne dictionary
#the dictionary is from: https://blog.koheiw.net/?p=79 
# from amir: it looks like this is a dictionary of words that are used in discussions of migration policy that are given + and - scores 
# to capture the + and - sentiments towards migration. Most of these words seem to have nothing directly to do with migration, but imply 
# + or - sentiments when used in the context of migration policy

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
  select(-c(iso3country, party.facts.id, parliament)) %>%
  filter(date>"2009-12-20")

#turn into corpus
speechcorp <- corpus(speeches)

#summary
summary(speechcorp)
head(summary(speechcorp))

#restrict to speeches with over 50 words
#clean_corp <- corpus_subset(speechcorp, ntoken(speechcorp) > 100)
#might we also want to cut of speaches that are extremely long??

#dfm creates a document feature matrix with immigration policy words
# Amir: if the idea is to identify migrant-related words, this would be great to do with a dictionary of terms like migra*, refug*, and asyl* 
# and that includes longer terms like those listed here: https://www.iom.int/key-migration-terms
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
# Amir: I think there already is a party variable in the speeches data
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


