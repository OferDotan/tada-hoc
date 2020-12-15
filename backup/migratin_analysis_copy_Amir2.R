#load packages
library(readr)
library(quanteda)
library(quanteda.textmodels)
library(readtext)
library(tidyverse)
library(data.table)
library(stm)
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

Corp_HouseOfCommons_V2  <- readRDS("Corp_HouseOfCommons_V2.rds")
names(Corp_HouseOfCommons_V2)

##############
# SUBSETTING #
##############

# limitation: 1. loosing short responses (responses in general); 2. would not include any documents not mentioning "our terms" in either agenda or text

#year: subset to speeches from 2010 (Justification:Tory manifesto)
speeches <- Corp_HouseOfCommons_V2 %>% 
  select(!c(iso3country, party.facts.id, parliament)) %>%
  filter(date>"2009-12-20")

  #alternative for Amir
speeches <- Corp_HouseOfCommons_V2 %>% 
  select(-c(iso3country, party.facts.id, parliament)) %>%
  filter(date>"2009-12-20")

#content/terms: subset to speeches that either contain or termed (agenda) as "immigra*", "refugee*" or "asylum" (according to v.D)

toMatch <- c("immigra*","Immigra*","refugee*","Refugee*","asylum","Asylum", " migra*"," Migra*")
agenda_text_filter <- filter(speeches, grepl(paste(toMatch,collapse="|"), agenda) | grepl(paste(toMatch,collapse="|"), text))

# create corpus

speechcorp <- corpus(agenda_text_filter)
names(agenda_text_filter)
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

# fit a simple model
mod <- stm(speech_dfm, K = 20, seed = 12345)
topic_labels <- labelTopics(mod)
plot(mod, type = "labels", labeltype = "prob") # or frex, lift, score

# wordcloud
cloud(mod, topic = 6, max_words = 90, color = c('blue','purple','orange'))

### Visualization: topic proportions by party 

# Filtering out empty documents from speech_dfm to match the documents selected by STM so that data can be combined
speech_dfm_subs <- dfm_subset(speech_dfm, rowSums(speech_dfm)>0)

# combine stm thetas with dfm docvars
df_theta <- as.data.frame(mod$theta)%>%
  cbind(docvars(speech_dfm_subs))

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


####Descriptives
# line plot: topic evolution over time (time = X, topic = Y) 
names(df_theta)
# plotting thetas (y) and dates (x)
ggplot(df_theta,aes(x=date,color=topic_names,y=thetas)) + 
  geom_point() +
  geom_line() +
  facet_grid(topic~origin)

head(mod$beta$logbeta)
##### topics over time
names(longdf_theta)

M <- gather(dft,topic,value,-id,-date,-origin) %>%
  group_by(topic,date,origin) %>%
  summarize(value=mean(value))

longdf_theta_mean <- group_by(topic,date) %>%
  summarize(thetamean=mean(theta))

##### topics over time
longdf_theta <- pivot_longer(df_theta, cols = names(df_theta[1:6]), names_to = "topic",values_to = "theta")

ggplot(longdf_theta,aes(x=date, y=theta)) + 
  geom_point() +
  geom_line() +
  facet_grid(topic ~ .)


plot(prep, "day", method = "continuous", topics = 1,
        + model = mod, printlegend = FALSE, xaxt = "n", xlab = "Time")
monthseq <- seq(from = as.Date("2008-01-01"),
                   + to = as.Date("2008-12-01"), by = "month")
R> monthnames <- months(monthseq)
R> axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
        + labels = monthnames)
# 1. 
# density ridge plot: frequency of migration words by topic (frequency of migration words = X, topic = Y) 


# we arent's interested in the text, just in the occurance of migration vocab in each document
agenda_text_filter_migwordfreq <- agenda_text_filter %>%
  mutate(migwordferq=length(toMatch))
toMatch
contain


agenda_text_filter_just_text <-  select(agenda_text_filter, speechnumber, text)
df_theta_w_text <- left_join(df_theta, agenda_text_filter_just_text, by="speechnumber")
head(df_theta_w_text)
names(df_theta_w_text)

# Filtering out empty documents from speech_dfm to match the documents selected by STM so that data can be combined
speech_dfm_subs <- dfm_subset(speech_dfm, rowSums(speech_dfm)>0)
head(mod$vocab)
docvars(speech_dfm)

names(docvars(speech_dfm))
speech_dfm
# docs with text as column
agenda_text_filter
agenda_text_filter_subs <- filter(agenda_text_filter, agenda_text_filter$text!="")
names(agenda_text_filter)
select
length(agenda_text_filter)

length(agenda_text_filter$text)

names(df_theta)
head(df_theta)

longdf_theta <- pivot_longer(df_theta, cols = names(df_theta[1:6]), names_to = "topic",values_to = "theta")


# ridgeplot showing each party's thetas
ridgeplot1 <- ggplot(longdf_theta, aes(x = theta, y = reorder(topic, -theta), fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  #scale_fill_gradient2(name = "RILE Position", low = "blue", high = "red") +
  labs(title = 'RILE Positions of Political Parties in Germany in 2013')
ridgeplot1
# migration words
toMatch

### for reference rom assingment 3

# ridgeplot showing each party's position in 2013. No transparency because it's not needed for seeing overlaps here
ridgeplot1_2013 <- ggplot(longsamps2013, aes(x = value, y = reorder(party, -value), fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_gradient2(name = "RILE Position", low = "blue", high = "red") +
  labs(title = 'RILE Positions of Political Parties in Germany in 2013')

ridgeplot1_2013


# plot: prevalence of immigration debates over time (vertical line at 07.05.2015 = general election & vertical line at 23.06.2016 = Brexit referendum)
library(lubridate)
agenda_text_filter$date <- as.Date(agenda_text_filter$date, format="%Y-%m-%d")
count_months = agenda_text_filter %>% group_by(month=floor_date(date, "month")) %>% summarise(frequency = n())

ggplot(count_months, aes(x=month, y=frequency))+
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2") +
  ggtitle("Prevalence of Immigration debates")+
  geom_vline(xintercept = as.Date("2015-05-07"), linetype = "dashed")+ #general election 2015
  geom_vline(xintercept = as.Date("2016-06-23"), linetype = "dashed", color = "red")+ # Brexit referendum
  theme(axis.text.x = element_text(angle = 90))

# ridgeline plot of party and topic
names(longdf_theta)
ridgeplot2 <- ggplot(longdf_theta, aes(x = topic, y = party, fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) #+
  #scale_fill_gradient2(name = "RILE Position", low = "blue", high = "red") +
  #labs(title = 'RILE Positions of Political Parties in Germany in 2013')
ridgeplot2



#########
names(longdf_theta)
newplot1 <- ggplot(longdf_theta, aes(x = theta, y = date)) + 
  geom_line()#+
  #facet_grid(longdf_theta$topic)
  #geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) #+
#scale_fill_gradient2(name = "RILE Position", low = "blue", high = "red") +
#labs(title = 'RILE Positions of Political Parties in Germany in 2013')
newplot1
# do this by topic
names(agenda_text_filter)
names(longdf_theta)

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
# do this py party
# do this by topic

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





