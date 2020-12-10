#load packages
library(readr)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.sentiment)
library(readtext)
library(tidyverse)
library(data.table)
library(stm)
library(dplyr)
library(ggplot2)
library(lubridate)
library(lme4)
library(lattice)

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

toMatch <- c("immigra*","Immigra*","refugee*","Refugee*","asylum","Asylum"," migra*"," Migra*")
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



################
# Topic Models #
################

# fit a simple model with 6 topics
mod_1 <- stm(speech_dfm, K = 6, seed = 12345)
labelTopics(mod_1)
plot(mod_1, type = "labels", labeltype = "prob") # or frex, lift, score

# check if topics are exclusive
dotchart(exclusivity(mod_1), labels = 1:6)

# check if topics are coherent
cohere <- semanticCoherence(mod, speech_dfm)
dotchart(cohere, labels = 1:6) 

# fit a simple model with 12 topics
mod_2 <- stm(speech_dfm, K = 12, seed = 12345)
labelTopics(mod_2)
plot(mod_2, type = "labels", labeltype = "prob") # or frex, lift, score

# check if topics are exclusive 
dotchart(exclusivity(mod_2), labels = 1:12)

#check if topics are coherent
cohere <- semanticCoherence(mod_2, speech_dfm)
dotchart(cohere, labels = 1:12)

########
# KWIC #
########

kw_immigration <- kwic(speechcorp, paste(toMatch,collapse="|"), window = 20)

kwic_df <- tibble(speaker = kw_immigration$docname, 
                  text = paste(kw_immigration$pre, kw_immigration$post, sep = " "))
corp_kwic <- corpus(kwic_df)
summary(corp_kwic)

# turn kwic into dfm

kwic_dfm <- dfm(corp_kwic, 
                remove_punct = TRUE,
                remove = stopwords(),
                remove_symbols = TRUE,
                remove_separators = TRUE,
                split_hyphens = TRUE,
                remove_numbers = TRUE)

# wordcloud
textplot_wordcloud(kwic_dfm, max_words = 90, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))


## removing also key words to see what is left
key_words <- c("immigra*","Immigra*","refugee*","Refugee*","asylum","Asylum"," migra*"," Migra*")
kwic_dfm_no_key <- dfm(corp_kwic,
                       remove_punct = TRUE,
                       remove = c(as.vector(key_words),stopwords()),
                       remove_symbols = TRUE,
                       remove_separators = TRUE,
                       split_hyphens = TRUE,
                       remove_numbers = TRUE)

# wordcloud no key words
textplot_wordcloud(kwic_dfm_no_key, max_words = 90, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))


################
# DESCRIPTIVES #
################
# plot: prevalence of immigration debates over time (vertical line at 07.05.2015 = general election & vertical line at 23.06.2016 = Brexit referendum)

agenda_text_filter$date <- as.Date(agenda_text_filter$date, format="%Y-%m-%d")
count_months = agenda_text_filter %>% group_by(month=floor_date(date, "month")) %>% summarise(frequency = n())

ggplot(count_months, aes(x=month, y=frequency))+
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2") +
  ggtitle("Prevalence of Immigration debates")+
  geom_vline(xintercept = as.Date("2015-05-07"), linetype = "dashed")+ #general election 2015
  geom_vline(xintercept = as.Date("2016-06-23"), linetype = "dashed", color = "red")+ # Brexit referendum
  theme(axis.text.x = element_text(angle = 90))




# density ridge plot: topic evolution over time (time = X, topic = Y) 
## Hypo: we see more debates before events like election/referendum





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


sentiment <- textstat_polarity(speechcorp, 
                               data_dictionary_LSD2015)

hist(sentiment$sentiment, breaks = 60)
sentiment$sent_prob <- 1/(1 + exp(-sentiment$sentiment))

#compare with kwic corpus

kwic_sentiment <- textstat_polarity(corp_kwic, 
                               data_dictionary_LSD2015)

hist(kwic_sentiment$sentiment, breaks = 60)
kwic_sentiment$sent_prob <- 1/(1 + exp(-kwic_sentiment$sentiment))

# add sentiments to initial data frames
agenda_text_filter = cbind(agenda_text_filter,sentiment)
kw_immigration = cbind(kw_immigration,kwic_sentiment)

# graph sentiment over time for overall subset

agenda_text_filter$date <- as.Date(agenda_text_filter$date, format="%Y-%m-%d")
sentiment_df <- data.frame(date = agenda_text_filter$date, sentiment = agenda_text_filter$sentiment)
sentiment_month = sentiment_df %>% group_by(month=floor_date(date, "month")) %>% summarise(avg_sentiment = mean(sentiment))


ggplot(sentiment_month, aes(x=month, y=avg_sentiment))+
  geom_area( fill="#69b3a2", alpha=0.4, aes(group=1)) +
  geom_line(color="#69b3a2",aes(group=1)) +
  ggtitle("Observed Sentiment in immigration related contributions overall")+
  geom_vline(xintercept = as.Date("2015-05-07"), linetype = "dashed")+ #general election 2015
  geom_vline(xintercept = as.Date("2016-06-23"), linetype = "dashed", color = "red")+ # Brexit referendum
  theme(axis.text.x = element_text(angle = 90))

# graph sentiment over time for kwic subset ## currently no date for kwic

#check if doc_id is unique
length(unique(agenda_text_filter$doc_id)) # yes. 

# add date column to kwic
doc_id_date <- data.frame(date = agenda_text_filter$date,docname = agenda_text_filter$doc_id) 
kw_imm_date <- merge(kw_immigration,doc_id_date,by = "docname", all.x = TRUE, sort = FALSE) # will need to merge based on the docname, as this is the same as the initial doc_id.


kw_imm_date$date <- as.Date(kw_imm_date$date, format="%Y-%m-%d")
kw_sentiment_df <- data.frame(date = kw_imm_date$date, sentiment = kw_imm_date$sentiment)
kw_sentiment_month = kw_sentiment_df %>% group_by(month=floor_date(date, "month")) %>% summarise(avg_sentiment = mean(sentiment))


ggplot(kw_sentiment_month, aes(x=month, y=avg_sentiment))+
  geom_area( fill="#69b3a2", alpha=0.4, aes(group=1)) +
  geom_line(color="#69b3a2",aes(group=1)) +
  ggtitle("Observed Sentiment in Context of Keywords in immigration related contributions")+
  geom_vline(xintercept = as.Date("2015-05-07"), linetype = "dashed")+ #general election 2015
  geom_vline(xintercept = as.Date("2016-06-23"), linetype = "dashed", color = "red")+ # Brexit referendum
  theme(axis.text.x = element_text(angle = 90))








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





