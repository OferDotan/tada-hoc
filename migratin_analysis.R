#load packages
# library(readr)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.sentiment)
library(quanteda.textstats)
library(readtext)
library(tidyverse)
# library(data.table)
library(stm)
library(lubridate)
library(lme4)
library(lattice)
library(wordcloud)
library(ggridges)
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

# think about removing single word answers (or above 5)

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
#plot(mod_1, type = "labels", labeltype = "prob") # or frex, lift, score

# wordcloud
cloud(mod, topic = 6, max_words = 90, color = c('blue','purple','orange'))

# check if topics are exclusive
dotchart(exclusivity(mod_1), labels = 1:6)

# check if topics are coherent
cohere <- semanticCoherence(mod, speech_dfm)
dotchart(cohere, labels = 1:6) 

### Visualization: topic proportions by party 

# Filtering out empty documents from speech_dfm to match the documents selected by STM so that data can be combined
speech_dfm_subs <- dfm_subset(speech_dfm, rowSums(speech_dfm)>0)

# combine stm thetas with dfm docvars
df_theta <- as.data.frame(mod$theta)%>%
  cbind(docvars(speech_dfm_subs))

### naming topics
#combine first 3 labels as a string for a new label
topic_names <- c()
for (topicnumber in topic_labels$topicnums){
  first_3<-(str_c(topic_labels$frex[topicnumber,1], 
                  ", " , topic_labels$frex[topicnumber,2], 
                  ", " , topic_labels$frex[topicnumber,3]))
  topic_names<- append(topic_names, first_3)
}

# renaming columns
names(df_theta)[1:6] <- topic_names
###

# grouping by party and generating means 
df_party <- df_theta %>%
  group_by(party)

df_party_mean <- aggregate(df_party[, 1:6], list(df_party$party), mean)
names(df_party_mean)[1] <- "Party" 

# Turning into a table of proportions
df_party_prop <- df_party_mean[1] %>%
  cbind(as.data.frame(round(100*prop.table(df_party_mean[2:7]),digits=2)))

# pivoting df for making figure
df_party_prop_longer <-df_party_prop %>%
  pivot_longer(c(2:7), names_to = "Topic", values_to = "Proportion")

# visualizing proportions of topics by party
party_topic_plot1 <- df_party_prop_longer %>%
  ggplot(aes(Party, Proportion, fill = Topic)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
party_topic_plot1

##### Visualization: topic prevalence over time 
longdf_theta <- pivot_longer(df_theta, cols = names(df_theta[1:6]), names_to = "topic",values_to = "theta")

ggplot(longdf_theta,aes(x=date, y=theta)) + 
  geom_point() +
  geom_line() +
  facet_grid(topic ~ .)

## ideas: - use means or proportions of thetas instead of thetas themselves
##        - aggregate by month instead of using simple date
##        - possibly color by party?

-----------------------
#### for 12 topics ####
-----------------------
  
# fit a simple model with 12 topics
mod_2 <- stm(speech_dfm, K = 12, seed = 12345)
labelTopics(mod_2)
#plot(mod_2, type = "labels", labeltype = "prob") # or frex, lift, score

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


# test <- convert(kwic_dfm_no_key, to = "data.frame", omit_empty = TRUE, docid_field = "doc_id", docvars = NULL) STILL NEEDED: Remove stopwords and keywords and transform back into dataframe (kwic_df)


# wordcloud incl. keywords
textplot_wordcloud(kwic_dfm, max_words = 90, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))


## removing also key words to see what is left
kwic_dfm_no_key <- dfm(corp_kwic,
                       remove_punct = TRUE,
                       remove = c(as.vector(toMatch),stopwords()),
                       remove_symbols = TRUE,
                       remove_separators = TRUE,
                       split_hyphens = TRUE,
                       remove_numbers = TRUE)

# wordcloud no key words
textplot_wordcloud(kwic_dfm_no_key, max_words = 90, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))

# plot frequency of words related to migration (including key terms)
features_dfm_inaug <- textstat_frequency(speech_dfm, n=100)
# Sort by reverse frequency order
features_dfm_inaug$feature <- with(features_dfm_inaug, reorder(feature, -frequency))

ggplot(features_dfm_inaug, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################
# DESCRIPTIVES #
################
# plot: prevalence of immigration debates over time by month | counting documents
agenda_text_filter$date <- as.Date(agenda_text_filter$date, format="%Y-%m-%d")
count_months = agenda_text_filter %>% group_by(month=floor_date(date, "month")) %>% summarise(frequency = n()) # for now this counts the number of documents in each month. Might need to make changes to this still. Could also count by agenda point = number of debates. 

ggplot(count_months, aes(x=month, y=frequency))+
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2") +
  labs(title = "Prevalence of immigration debates",
       subtitle = "by number of documents")+
  geom_vline(xintercept = as.Date("2015-05-07"), linetype = "dashed")+ #general election 2015
  geom_vline(xintercept = as.Date("2016-06-23"), linetype = "dashed", color = "red")+ # Brexit referendum
  theme(axis.text.x = element_text(angle = 90))

# plot: prevalence of immigration debates over time by month | counting unique agenda points
agenda_text_filter$date <- as.Date(agenda_text_filter$date, format="%Y-%m-%d")
count_months_agenda = agenda_text_filter %>% group_by(month=floor_date(date, "month"), agenda = agenda) %>% summarise(frequency = n()) # for now this counts the number of documents in each month. Might need to make changes to this still. Could also count by agenda point = number of debates. 
count_agenda = count_months_agenda %>% group_by(month=floor_date(month, "month")) %>% summarise(frequency = n()) # for now this counts the number of documents in each month. Might need to make changes to this still. Could also count by agenda point = number of debates. 

ggplot(count_agenda, aes(x=month, y=frequency))+
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2") +
  labs(title = "Prevalence of immigration debates",
       subtitle = "by number of agenda points (unique debates)")+
  geom_vline(xintercept = as.Date("2015-05-07"), linetype = "dashed")+ #general election 2015
  geom_vline(xintercept = as.Date("2016-06-23"), linetype = "dashed", color = "red")+ # Brexit referendum
  theme(axis.text.x = element_text(angle = 90))

# plot: prevalence of immigration debates over time by month | total number of words as a proxy for time spent on debating. 

agenda_text_filter$date <- as.Date(agenda_text_filter$date, format="%Y-%m-%d")
count_months_words = agenda_text_filter %>% group_by(month=floor_date(date, "month")) %>% summarise(word_sum = sum(terms)) # for now this counts the number of documents in each month. Might need to make changes to this still. Could also count by agenda point = number of debates. 

ggplot(count_months_words, aes(x=month, y=word_sum))+
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2") +
  labs(title = "Prevalence of immigration debates",
       subtitle = "by amounts of words (proxy for time spent on debates)")+
  geom_vline(xintercept = as.Date("2015-05-07"), linetype = "dashed")+ #general election 2015
  geom_vline(xintercept = as.Date("2016-06-23"), linetype = "dashed", color = "red")+ # Brexit referendum
  theme(axis.text.x = element_text(angle = 90))





# density ridge plot: topic evolution over time (time = X, topic = Y) 
## Hypo: we see more debates before events like election/referendum



#############################
## SENTIMENT | Data Frames ##
#############################

# sentiment based on overall corpus
sentiment <- textstat_polarity(speechcorp, 
                               data_dictionary_LSD2015)
sentiment$sent_prob <- 1/(1 + exp(-sentiment$sentiment))

hist(sentiment$sentiment, breaks = 60) # just for ourselves to see what's what

# sentiment based on kwic corpus

kwic_sentiment <- textstat_polarity(corp_kwic, 
                                    data_dictionary_LSD2015)
kwic_sentiment$sent_prob <- 1/(1 + exp(-kwic_sentiment$sentiment))
hist(kwic_sentiment$sentiment, breaks = 60) # just for ourselves to see what's what

# add sentiments to initial data frames
agenda_text_filter = cbind(agenda_text_filter,sentiment)
kw_immigration = cbind(kw_immigration,kwic_sentiment)

########################
## SENTIMENT | GRAPHS ##
########################

## OVERALL sentiment

agenda_text_filter$date <- as.Date(agenda_text_filter$date, format="%Y-%m-%d")
sentiment_df <- data.frame(date = agenda_text_filter$date, sentiment = agenda_text_filter$sentiment)
sentiment_month = sentiment_df %>% group_by(month=floor_date(date, "month")) %>% summarise(avg_sentiment = mean(sentiment))


ggplot(sentiment_month, aes(x=month, y=avg_sentiment))+
  geom_area( fill="#69b3a2", alpha=0.4, aes(group=1)) +
  geom_line(color="#69b3a2",aes(group=1)) +
  labs(title = "Observed Sentiment in immigration related contributions overall",
       caption = "dashed line (black = 2015 general election, red = Brexit referendum")+
  geom_vline(xintercept = as.Date("2015-05-07"), linetype = "dashed")+ #general election 2015
  geom_vline(xintercept = as.Date("2016-06-23"), linetype = "dashed", color = "red")+ # Brexit referendum
  theme(axis.text.x = element_text(angle = 90))

## OVERALL sentiment by party

agenda_text_filter$date <- as.Date(agenda_text_filter$date, format="%Y-%m-%d")
sentiment_party_df <- data.frame(date = agenda_text_filter$date, party = agenda_text_filter$party ,sentiment = agenda_text_filter$sentiment)
sentiment_party = sentiment_party_df %>% group_by(month=floor_date(date, "month"), party = party) %>% summarise(avg_sentiment = mean(sentiment))

ggplot(sentiment_party)+
  geom_line(aes(x=month, y=avg_sentiment,group = party), colour = "grey",size = 1) +
  geom_line(data = subset(sentiment_party, party == "Con") ,aes(x=month, y=avg_sentiment,group = party), colour = "red",size = 1) +
  geom_line(data = subset(sentiment_party, party == "Lab") ,aes(x=month, y=avg_sentiment,group = party), colour = "blue",size = 1) +
  labs(title = "Observed Sentiment in immigration related contributions overall by party",
       subtitle = "Conservative = red, Labour = blue",
       caption = "dashed line (black = 2015 general election, red = Brexit referendum")+
  geom_vline(xintercept = as.Date("2015-05-07"), linetype = "dashed")+ #general election 2015
  geom_vline(xintercept = as.Date("2016-06-23"), linetype = "dashed", color = "red")+ # Brexit referendum
  theme(axis.text.x = element_text(angle = 90))


## KWIC sentiment

length(unique(agenda_text_filter$doc_id)) # for merge, check if doc_id is unique --> yes. 
doc_id_date <- data.frame(date = agenda_text_filter$date,docname = agenda_text_filter$doc_id, party = agenda_text_filter$party) # add date column to kwic
kw_imm_date <- merge(kw_immigration,doc_id_date,by = "docname", all.x = TRUE, sort = FALSE) # will need to merge based on the docname, as this is the same as the initial doc_id.

kw_imm_date$date <- as.Date(kw_imm_date$date, format="%Y-%m-%d")
kw_sentiment_df <- data.frame(date = kw_imm_date$date, sentiment = kw_imm_date$sentiment)
kw_sentiment_month = kw_sentiment_df %>% group_by(month=floor_date(date, "month")) %>% summarise(avg_sentiment = mean(sentiment))

ggplot(kw_sentiment_month, aes(x=month, y=avg_sentiment))+
  geom_area( fill="#69b3a2", alpha=0.4, aes(group=1)) +
  geom_line(color="#69b3a2",aes(group=1)) +
  labs(title = "Observed Sentiment in Context of Keywords in immigration related contributions",
       caption = "dashed line (black = 2015 general election, red = Brexit referendum")+
  geom_vline(xintercept = as.Date("2015-05-07"), linetype = "dashed")+ #general election 2015
  geom_vline(xintercept = as.Date("2016-06-23"), linetype = "dashed", color = "red")+ # Brexit referendum
  theme(axis.text.x = element_text(angle = 90))

## KWIC sentiment by party

kw_imm_date$date <- as.Date(kw_imm_date$date, format="%Y-%m-%d")
kw_sentiment__party_df <- data.frame(date = kw_imm_date$date, party = kw_imm_date$party,sentiment = kw_imm_date$sentiment)
kw_sentiment_party = kw_sentiment__party_df %>% group_by(month=floor_date(date, "month"), party = party) %>% summarise(avg_sentiment = mean(sentiment))

ggplot(kw_sentiment_party)+
  geom_line(aes(x=month, y=avg_sentiment,group = party), colour = "grey",size = 1) +
  geom_line(data = subset(kw_sentiment_party, party == "Con") ,aes(x=month, y=avg_sentiment,group = party), colour = "red",size = 1) +
  geom_line(data = subset(kw_sentiment_party, party == "Lab") ,aes(x=month, y=avg_sentiment,group = party), colour = "blue",size = 1) +
  labs(title = "Observed Sentiment in in context of keyword immigration related contributions by party",
       subtitle = "Conservative = red, Labour = blue",
       caption = "dashed line (black = 2015 general election, red = Brexit referendum")+
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

## just to check what the sentiment analysis does: 

text <- data.frame(text =c("nice","evil","and"))
text <- corpus(text)
sent_text <- textstat_polarity(text, 
                               data_dictionary_LSD2015)


#### Sentiment only for refugee* by party
refu <- c("refugee*","Refugee*")
refugee <- filter(speeches, grepl(paste(refu,collapse="|"), agenda) | grepl(paste(refu,collapse="|"), text))
refcorp <- corpus(refugee)

ref_sent <- textstat_polarity(refcorp, 
                              data_dictionary_LSD2015)
ref_sent$sent_prob <- 1/(1 + exp(-ref_sent$sentiment))

refugee = cbind(refugee,ref_sent)

refugee$date <- as.Date(refugee$date, format="%Y-%m-%d")
ref_party_df <- data.frame(date = refugee$date, party = refugee$party ,sentiment = refugee$sentiment)
ref_party = ref_party_df %>% group_by(month=floor_date(date, "month"), party = party) %>% summarise(avg_sentiment = mean(sentiment))

ggplot(ref_party)+
  geom_line(aes(x=month, y=avg_sentiment,group = party), colour = "grey",size = 1) +
  geom_line(data = subset(ref_party, party == "Con") ,aes(x=month, y=avg_sentiment,group = party), colour = "red",size = 1) +
  geom_line(data = subset(ref_party, party == "Lab") ,aes(x=month, y=avg_sentiment,group = party), colour = "blue",size = 1) +
  labs(title = "Observed Sentiment in refugee*- related contributions overall by party",
       subtitle = "Conservative = red, Labour = blue",
       caption = "dashed line (black = 2015 general election, red = Brexit referendum")+
  geom_vline(xintercept = as.Date("2015-05-07"), linetype = "dashed")+ #general election 2015
  geom_vline(xintercept = as.Date("2016-06-23"), linetype = "dashed", color = "red")+ # Brexit referendum
  theme(axis.text.x = element_text(angle = 90))






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

####### references for sentiment


# how does sentiment evolve over time? 
## H1: Overall, sentiments towards immigration became more negative, peaking in 2015 & 2016 
# sentiment by party
# sentrement (line graph) 
# migrants as passive "bystanders" vs. active 

# Amir: This one might be more relevant to us: http://www.snsoroka.com/data-lexicoder/ 
# although it is actually designed for use on political speech (often related to migration) [but] on social meda...
# for reference, see https://www.tandfonline.com/doi/full/10.1080/1369183X.2019.1665990 (section 3.2. Measuring the sentiment of migration discourse on Facebook)
# for another description of what the Lexicoder does, see https://ijoc.org/index.php/ijoc/article/view/6273/1946, page 975







