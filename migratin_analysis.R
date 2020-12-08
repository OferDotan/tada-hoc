#load packages
library(readr)
library(quanteda)
library(quanteda.textmodels)
library(readtext)
library(tidyverse)
library(data.table)
library(stm)

########
# Brainstorm: Research Questions. 

# Which issues related to immigration should be prioritized? (priotisation based on contentiousness --> extreme (+|-) divergence of sentiments from the mean)
# Prevalence of sentiment across parties. 
# How do topics shape sentiments? (correlation of topics and sentiments in parliamentary discourse related to immigration) 

# 2x2 Matrix: Threat vs. Need | Active vs Passive
# Observable shifts from positive passive (dependent, needy migrants) to positive/active (people with value to be included). 
# At the same time: Shift from negative/active (security and cultural threat) to negative/passive (economic and social weight).
# Hypo: We'll assume to see a shift from X to Y ... and operationalise this by looking at active vs. passive. 

#############
# LOAD DATA #
#############

Corp_HouseOfCommons_V2  <- readRDS("~/Desktop/tada-hoc/Corp_HouseOfCommons_V2.rds")
names(Corp_HouseOfCommons_V2)

##############
# SUBSETTING #
##############

# limitation: 1. loosing short responses (responses in general); 2. would not include any documents not mentioning "our terms" in either agenda or text

#year: subset to speeches from 2010 (Justification:Tory manifesto)
speeches <- Corp_HouseOfCommons_V2 %>% 
  select(!c(iso3country, party.facts.id, parliament)) %>%
  filter(date>"2009-12-20")

#content/terms: subset to speeches that either contain or termed (agenda) as "immigra*", "refugee*" or "asylum" (according to v.D)

toMatch <- c("immigra*","Immigra*","refugee*","Refugee*","asylum","Asylum", " migra*"," Migra*")
agenda_text_filter <- filter(speeches, grepl(paste(toMatch,collapse="|"), agenda) | grepl(paste(toMatch,collapse="|"), text))

# create corpus

speechcorp <- corpus(agenda_text_filter)

# create dfm
speech_dfm <- dfm(speechcorp, 
                  remove_punct = TRUE,
                  remove = stopwords(),
                  remove_symbols = TRUE,
                  remove_separators = TRUE,
                  split_hyphens = TRUE,
                  remove_numbers = TRUE)


########
# KWIC #
########
# Write code in a way to make sure that bubbles of individual keywords don't overlap and double count. 

kw_immigration <- kwic(speechcorp, paste(toMatch,collapse="|"), window = 20)
install.packages("xtable")

kwic_df <- tibble(speaker = kw_immigration$docname, 
                  text = paste(kw_immigration$pre, kw_immigration$post, sep = " "))
corp_kwic <- corpus(kwic_df)
summary(corp_kwic)

# here: think about including a step that excludes all keywords. THis is as we don't want those to show in our wordcloud. 

# turn kwic into dfm

kwic_dfm <- dfm(corp_kwic, 
                remove_punct = TRUE,
                remove = stopwords(),
                remove_symbols = TRUE,
                remove_separators = TRUE,
                split_hyphens = TRUE,
                remove_numbers = TRUE)

# wordcloud
textplot_wordcloud(kwic_dfm, max_words = 90, color = c('blue','purple','orange'))



################
# DESCRIPTIVES #
################
# density plot: prevalence of immigration debates over time (vertical line at 07.05.2015 = general election & vertical line at 23.06.2016 = Brexit referendum)
# density ridge plot: topic evolution over time (time = X, topic = Y) 
## Hypo: we see more debates before events like election/referendum






################
# Topic Models #
################
img_dfm <- dfm(speechcorp, 
               remove_punct = TRUE,
               remove = stopwords(),
               remove_symbols = TRUE,
               remove_separators = TRUE,
               split_hyphens = TRUE,
               remove_numbers = TRUE)

##Topic modeling 
# fit a simple model
mod <- stm(img_dfm, K = 6, seed = 12345)
labelTopics(mod)
plot(mod, type = "labels", labeltype = "prob") # or frex, lift, score





#############
# Sentiment #
#############
# Amir: This one might be more relevant to us: http://www.snsoroka.com/data-lexicoder/ 
# although it is actually designed for use on political speech (often related to migration) [but] on social meda...
# for reference, see https://www.tandfonline.com/doi/full/10.1080/1369183X.2019.1665990 (section 3.2. Measuring the sentiment of migration discourse on Facebook)
# for another description of what the Lexicoder does, see https://ijoc.org/index.php/ijoc/article/view/6273/1946, page 975

# how does sentiment evolve over time? 
## H1: Overall, sentiments towards immigration became more negative, peaking in 2015 & 2016 
# sentiment by party
# sentrement (line graph) 
# migrants as passive "bystanders" vs. active 





###############
# Exploratory #
###############


# some examples of agendas
head(unique(Corp_HouseOfCommons_V2$agenda))
#number of agendas
length(unique(Corp_HouseOfCommons_V2$agenda))
length(Corp_HouseOfCommons_V2$agenda)
Corp_HouseOfCommons_V2$agenda["migration"]
?contains()




###### testing the subsets:

#dfm creates a document feature matrix with immigration policy words
# Amir: if the idea is to identify migrant-related words, this would be great to do with a dictionary of terms like migra*, refug*, and asyl* 
# and that includes longer terms like those listed here: https://www.iom.int/key-migration-terms



#toMatch2 <- c("immigra*","Immigra*","refugee*","Refugee*","asylum","Asylum","migra*","Migra*")
#agenda_text_filter_2 <- filter(speeches, grepl(paste(toMatch2,collapse="|"), agenda) | grepl(paste(toMatch2,collapse="|"), text))

#toMatch3 <- c("migra*","Migra*")
#agenda_text_filter_3 <- filter(speeches, grepl(paste(toMatch3,collapse="|"), agenda) | grepl(paste(toMatch3,collapse="|"), text))

#toMatch4 <- c("immigra*","Immigra*")
#agenda_text_filter_4 <- filter(speeches, grepl(paste(toMatch4,collapse="|"), agenda) | grepl(paste(toMatch4,collapse="|"), text))

#agenda_filter <- with(speeches, speeches[ grepl(paste(toMatch,collapse="|"), agenda), ])
#agenda_text_filter <- with(speeches, speeches[grepl(paste(toMatch,collapse="|"), agenda) | grepl(paste(toMatch,collapse="|"), text), ])


#test = with(speeches, speeches[ grepl( 'immigra*', agenda) | grepl( 'Immigra*', agenda), ])

#test1 = with(speeches, speeches[ grepl( 'immigra*|Immigra*', agenda), ])

#test2 = with(speeches, speeches[ grepl( 'immigra*|Immigra*|refugee*|Refugee*|asylum|Asylum', agenda), ])





