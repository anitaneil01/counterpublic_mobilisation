require(rvest)
require(tidyverse)
require(rio)
require(quanteda)
library(readr)
library(dplyr)
require(igraph)
require(stringr)
library(plyr)


##### Preparation for community detection #####

cp_data <- readRDS('./Twitter_Data.rds')
cp_data

## Defining user as nodes
cp_nodes <- cp_data$user
cp_nodes

## Detecting movement related hashtags
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch')) -> cp_hash

## Sort df according to the three movements
cp_hash %>% group_by(metoo, timesup, womensmarch) %>% tally

## which user uses hashtags and which do not
cp_hash %>% group_by(user) %>% summarise(metoo = any(metoo), timesup = any(timesup), womensmarch = any(womensmarch)) %>% 
  select(user, metoo, timesup, womensmarch) %>% ungroup %>%
  group_by(metoo, timesup, womensmarch) %>% tally

## retweet network with hashtags - creating edge list
cp_data %>% filter(str_detect(text, '^RT')) -> cp_rt

cp_rt %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                 timesup = str_detect(lower_text, '#timesup'), 
                 womensmarch = str_detect(lower_text, '#womensmarch')) %>% filter(metoo) %>%
  mutate(src = str_extract(text, 'RT [a-zA-Z0-9_]+'), src = str_remove(src, '^RT ')) %>% 
  select(src, user) %>% group_by(src, user) %>% tally %>% ungroup %>% rename(weight = 'n')-> cp_edge_list

graph_from_data_frame(cp_edge_list) -> cp_graph


##### H2 - extracting all hashtags #####
require(rtweet)

extract.hashes = function(vec){
  
  hash.pattern = "#[[:alpha:]]+"
  have.hash = grep(x = vec, pattern = hash.pattern)
  
  hash.matches = gregexpr(pattern = hash.pattern,
                          text = vec[have.hash])
  extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
  
  df = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) = c("tag","freq")
  df = df[order(df$freq,decreasing = TRUE),]
  return(df)
}

twitter_text = cp_data$text
dat = head(extract.hashes(twitter_text),50)
dat2 = transform(dat,tag = reorder(tag,freq))
View(dat2)

## Visualize 50 most frequent hashtags in our data
require(ggplot2)
p = ggplot(dat2, aes(x= tag, y = freq)) + geom_bar(stat = "identity", fill = "blue")
p + coord_flip() + labs(title = "Hashtag frequencies in the tweets")

##Testing other possible combinations of our movements with other movement related hashtags
#Combination 1: #metoo, #timesup #womensmarch, whywewearblack 
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), whywewearblack = str_detect(lower_text, '#whywewearblack')) -> cp_hash1
cp_hash1 %>% group_by(metoo, timesup, womensmarch, whywewearblack) %>% tally 


#Combination 2: #metoo, #timesup #womensmarch, #blacklivesmatter 
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), blm = str_detect(lower_text, '#blacklivesmatter')) -> cp_hash2
cp_hash2 %>% group_by(metoo, timesup, womensmarch, blm) %>% tally 


#Combination 3: #metoo, #timesup #womensmarch,#imstillwithher 
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), imstillwithher = str_detect(lower_text, '#imstillwithher')) -> cp_hash3
cp_hash3 %>% group_by(metoo, timesup, womensmarch, imstillwithher) %>% tally 


#Combination 4: #metoo, #timesup #womensmarch,#bluewave 
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), bluewave = str_detect(lower_text, '#bluewave')) -> cp_hash4
cp_hash4 %>% group_by(metoo, timesup, womensmarch, bluewave) %>% tally 


#Combination 5: #metoo, #timesup #womensmarch, #marchforourlives 
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), marchforourlives = str_detect(lower_text, '#marchforourlives')) -> cp_hash5

cp_hash5 %>% group_by(metoo, timesup, womensmarch, marchforourlives) %>% tally 


##Testing other possible combinations with feminist hashtags
#Combination 6: #metoo, #timesup #womensmarch, #resist
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), resist = str_detect(lower_text, '#resist')) -> cp_hash6
cp_hash6 %>% group_by(metoo, timesup, womensmarch, resist) %>% tally 

#Combination 7: #metoo, #timesup #womensmarch, #theresistance
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), theresistance = str_detect(lower_text, '#theresistance')) -> cp_hash7
cp_hash7 %>% group_by(metoo, timesup, womensmarch, theresistance) %>% tally 

##Combination 8: #metoo, #timesup #womensmarch, #oprah
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), oprah = str_detect(lower_text, '#oprah')) -> cp_hash8
cp_hash8 %>% group_by(metoo, timesup, womensmarch, oprah) %>% tally 

#Combination 9: #metoo, #timesup #womensmarch, #oktosay
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), oktosay = str_detect(lower_text, '#oktosay')) -> cp_hash9
cp_hash9 %>% group_by(metoo, timesup, womensmarch, oktosay) %>% tally 

##Combination 10: #metoo, #timesup #womensmarch, #iwd
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), iwd = str_detect(lower_text, '#iwd')) -> cp_hash10
cp_hash10 %>% group_by(metoo, timesup, womensmarch, iwd) %>% tally 

#Combination 11: #metoo, #timesup #womensmarch, #feminism 
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), feminism = str_detect(lower_text, '#feminism')) -> cp_hash11
cp_hash11 %>% group_by(metoo, timesup, womensmarch, feminism) %>% tally 

#Combination 12: #metoo, #timesup #womensmarch,#sexualharassment  
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), sexualharassment = str_detect(lower_text, '#sexualharassment ')) -> cp_hash12
cp_hash12 %>% group_by(metoo, timesup, womensmarch, sexualharassment) %>% tally 

#Combination 13: #metoo, #timesup #womensmarch, #womensrights  
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), womensrights = str_detect(lower_text, '#womensrights ')) -> cp_hash13
cp_hash13 %>% group_by(metoo, timesup, womensmarch, womensrights) %>% tally 

#Combination 14: #metoo, #timesup #womensmarch, #equality  
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), equality = str_detect(lower_text, '#equality ')) -> cp_hash14
cp_hash14 %>% group_by(metoo, timesup, womensmarch, equality) %>% tally 

#Combination 15: #metoo, #timesup #womensmarch, #neveragain  
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), neveragain = str_detect(lower_text, '#neveragain ')) -> cp_hash15
cp_hash15 %>% group_by(metoo, timesup, womensmarch, neveragain) %>% tally 

#Combination 16: #metoo, #timesup #womensmarch, #werise  
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), werise = str_detect(lower_text, '#werise ')) -> cp_hash16
cp_hash16 %>% group_by(metoo, timesup, womensmarch, werise) %>% tally 

#Combination 17: #metoo, #timesup #womensmarch, #ewomensday  
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), womensday = str_detect(lower_text, '#womensday ')) -> cp_hash17
cp_hash17 %>% group_by(metoo, timesup, womensmarch, womensday) %>% tally 

#Combination 18: #metoo, #timesup #womensmarch, #nastywomanvote  
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), nastywomanvote = str_detect(lower_text, '#nastywomanvote ')) -> cp_hash18
cp_hash18 %>% group_by(metoo, timesup, womensmarch, nastywomanvote) %>% tally 


##Testing other possible combinations with political hashtags
#Combination 19: #metoo, #timesup #womensmarch, #maga, #imstillwithher 

cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), maga = str_detect(lower_text, '#maga '), imstillwithher = str_detect(lower_text, '#imstillwithher ')) -> cp_hash19
cp_hash19 %>% group_by(metoo, timesup, womensmarch, maga, imstillwithher) %>% tally


##Testing other possible combinations with social concern hashtags
#Combination 20: #metoo, #timesup #womensmarch, #lgbtq

cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), lgbtq = str_detect(lower_text, '#lgbtq ')) -> cp_hash20
cp_hash20 %>% group_by(metoo, timesup, womensmarch, lgbtq) %>% tally 

#Combination 21: #metoo, #timesup #womensmarch, #mentalhealth
cp_data %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, '#metoo'), 
                   timesup = str_detect(lower_text, '#timesup'), 
                   womensmarch = str_detect(lower_text, '#womensmarch'), mentalhealth = str_detect(lower_text, '#mentalhealth ')) -> cp_hash21
cp_hash21 %>% group_by(metoo, timesup, womensmarch, mentalhealth) %>% tally 


##### Community detection with walktrap #####
cp_wc <- cluster_walktrap(cp_graph)

## 8 largest communities in our network
membership(cp_wc) %>% table %>% sort %>% tail(8) %>% names -> large_comm

V(cp_graph)$comm <- membership(cp_wc)
delete.vertices(cp_graph, which(!V(cp_graph)$comm %in% large_comm))

cp_hash %>% group_by(user) %>% summarise(metoo = any(metoo), timesup = any(timesup), womensmarch = any(womensmarch)) %>% 
  select(user, metoo, timesup, womensmarch) -> user_hash

##Inspecting the communities 
tibble(user= names(membership(cp_wc)), group = membership(cp_wc)) %>% left_join(user_hash) %>% 
  group_by(group, metoo, timesup, womensmarch) %>% tally %>% filter(group == 5) %>% arrange(n) #results for H2

tibble(user= names(membership(cp_wc)), group = membership(cp_wc)) %>% left_join(user_hash) %>% 
  group_by(group, metoo, timesup, womensmarch) %>% tally %>% filter(group == 51) %>% arrange(n)

tibble(user= names(membership(cp_wc)), group = membership(cp_wc)) %>% left_join(user_hash) %>% 
  group_by(group, metoo, timesup, womensmarch) %>% tally %>% filter(group == 48) %>% arrange(n)

tibble(user= names(membership(cp_wc)), group = membership(cp_wc)) %>% left_join(user_hash) %>% 
  group_by(group, metoo, timesup, womensmarch) %>% tally %>% filter(group == 61) %>% arrange(n)

tibble(user= names(membership(cp_wc)), group = membership(cp_wc)) %>% left_join(user_hash) %>% 
  group_by(group, metoo, timesup, womensmarch) %>% tally %>% filter(group == 63) %>% arrange(n)

write.graph(cp_graph, "cp_graph.gml", format = "gml")

## Calculating worthiness Version 1
cp_data %>% select(user, fullname, timestamp, likes, replies, retweets, text) %>% filter(user == "katyperry") -> katyperry_tweets
which(grepl("katyperry", cp_data$user)) ##looking up the row of the tweet in the data
cp_data[1794033,8]
cp_data %>% select(user, fullname, text) %>% filter(user == "BarackObama") #no result
cp_data %>% select(user, fullname, text) %>% filter(user == "taylorswift13") #no result
cp_data %>% select(user, fullname, text) %>% filter(user == "ladygaga") #no result
cp_data %>% select(user, fullname, timestamp, likes, replies, retweets, text) %>% filter(user == "TheEllenShow") ->theellenshow_tweets
cp_data %>% select(user, fullname, timestamp, likes, replies, retweets, text) %>% filter(user == "jtimberlake") -> jtimberlake_tweets
cp_data %>% select(user, fullname, text) %>% filter(user == "ArianaGrande") #no result
cp_data %>% select(user, fullname, text) %>% filter(user == "realDonaldTrump") #no result
cp_data %>% select(user, fullname, text) %>% filter(user == "KimKardashian") #no result
cp_data %>% select(user, fullname, text) %>% filter(user == "selenagomez") #no result
cp_data %>% select(user, fullname, text) %>% filter(user == "britneyspears") #no result
cp_data %>% select(user, fullname, text) %>% filter(user == "jimmyfallon") #no result
cp_data %>% select(user, fullname, text) %>% filter(user == "BillGates") #no result
cp_data %>% select(user, fullname, text) %>% filter(user == "JLo") #no result
cp_data %>% select(user, fullname, timestamp, likes, replies, retweets, text) %>% filter(user == "MileyCyrus") -> mileycyrus_tweets
cp_data %>% select(user, fullname, text) %>% filter(user == "KingJames") #no result
cp_data %>% select(user, fullname, text) %>% filter(user == "BrunoMars") #no result
cp_data %>% select(user, fullname, timestamp, likes, replies, retweets, text) %>% filter(user == "Oprah") -> oprah_tweets
cp_data %>% select(user, fullname, text) %>% filter(user == "KevinHart4real") #no result
cp_data %>% select(user, fullname, text) %>% filter(user == "wizkhalifa") #no result

worthiness <- rbind(katyperry_tweets, theellenshow_tweets, jtimberlake_tweets, mileycyrus_tweets, oprah_tweets)
worthiness %>% distinct(text, .keep_all = TRUE) -> worthiness_twitter
worthiness_twitter %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, 'metoo'), 
                              timesup = str_detect(lower_text, 'timesup'), 
                              womensmarch = str_detect(lower_text, 'womensmarch')) -> worthiness_movement
worthiness_movement %>% group_by(metoo, timesup, womensmarch) %>% tally

mean(worthiness_movement$likes) #21156,83
mean(worthiness_movement$replies) # 587,5
mean(worthiness_movement$retweets) #2634,667

worthiness_corpus <- corpus(worthiness_movement$lower_text)
docvars(worthiness_corpus, "user") <- worthiness_movement$user
docvars(worthiness_corpus, "retweets") <- worthiness_movement$retweets
docvars(worthiness_corpus, "timestamp") <- worthiness_movement$timestamp
summary(worthiness_corpus)

worthiness_dfm <- dfm(worthiness_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, remove_symbols = TRUE,remove = stopwords('en'))

## get frequency in all celebrity tweets
textstat_keyness(worthiness_dfm)
features_worthiness_dfm <- textstat_frequency(worthiness_dfm, n = 25)

## visualize
features_worthiness_dfm$feature <- with(features_worthiness_dfm, reorder(feature, -frequency))

ggplot(features_worthiness_dfm, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## show which user used which movements
worthiness_movement %>% group_by(user) %>% summarise(metoo = any(metoo), timesup = any(timesup), womensmarch = any(womensmarch)) %>% 
  select(user, metoo, timesup, womensmarch)

## Hashtag combination in political tweets
worthiness_movement %>% group_by(metoo, timesup, womensmarch) %>% tally


## Calculating worthiness Version 2

cp_hash %>% left_join(cp_wc_tibble) %>% group_by(user, group, lower_text) -> cp_groups
cp_groups %>% select(user, group, lower_text) %>% filter(group == 5) -> cp_group_5

# remove for empty document
cp_group_5 <- cp_group_5[-9765,]

worthiness_cp_group_5 <- dfm(corpus(cp_group_5$lower_text), remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("english"))
worthiness_cp_group_5_stm <- quanteda::convert(worthiness_cp_group_5, to = "stm")

saveRDS(worthiness_cp_group_5, "./worthiness_dfm_stm.RDS")
worthiness_cp_group_5_stm <- readRDS("./worthiness_dfm_stm.RDS")

require(stm)
library(stm)

stm_model <- stm(worthiness_cp_group_5_stm$documents, worthiness_cp_group_5_stm$vocab, K = 0, data = worthiness_cp_group_5_stm$meta, init.type = "Spectral", seed = 42)

saveRDS(stm_model, "./stm_model_worthiness.RDS")
stm_modeL_worthiness <- readRDS("./stm_model_worthiness.RDS")

labelTopics(stm_model_worthiness)
plot(stm_model_worthiness, type = "summary")

theta <- as.data.frame(stm_model_worthiness$theta)
colnames(theta) <- paste0("topic", 1:95)

worthiness_theta <- bind_cols(cp_group_5, theta)
worthiness_theta %>% arrange(desc(topic13)) %>% select(user, group, lower_text, topic13)
worthiness_theta %>% summarise(mean_theta = mean(topic13)) #0.0225
worthiness_theta %>% summarise(mean_theta = mean(topic90)) #0.0053


## Calculating unity on community level (H3)
cp_wc_tibble <- tibble(user = names(membership(cp_wc)), group = membership(cp_wc))
comm_df <- cp_hash %>% mutate(src = str_extract(text, 'RT [a-zA-Z0-9_]+')) %>% left_join(cp_wc_tibble) %>% 
  group_by(user) %>% select (group, user, timestamp, lower_text)

comm_df_clean <- na.omit(comm_df)

##look at hashtag frequencies for community 5 - unity
twitter_hash_text = comm5_df$lower_text
dat_hash = head(extract.hashes(twitter_hash_text),10)
dat2_hash = transform(dat_hash,tag = reorder(tag,freq))
p_hash = ggplot(dat2_hash, aes(x= tag, y = freq)) + geom_bar(stat = "identity", fill = "blue")
p_hash + coord_flip() + labs(title = "Hashtag frequencies community 5")


## Calculating numbers on community level (H3)
sum(str_count(cp_group_5$lower_text, "metoo")) #N=8844
sum(str_count(cp_group_5$lower_text, "timesup")) #N=3791
sum(str_count(cp_group_5$lower_text, "womensmarch")) #N=3608


## Calculating commitment 
## MeToo
cp_hash %>% left_join(cp_wc_tibble) %>% group_by(user, group) %>%
  summarise(metoo_freq = sum(metoo))

cp_hash %>% left_join(cp_wc_tibble) %>% group_by(user, group) %>%
  summarise(metoo_freq = sum(metoo)) %>% ungroup %>% 
  mutate(metoo0 = metoo_freq == 0, metoo1 = metoo_freq == 1, metoo_many = metoo_freq > 1)

## count users
cp_hash %>% left_join(cp_wc_tibble) %>% group_by(user, group) %>%
  summarise(metoo_freq = sum(metoo)) %>% ungroup %>% 
  mutate(metoo0 = metoo_freq == 0, metoo1 = metoo_freq == 1, metoo_many = metoo_freq > 1) %>% 
  group_by(group) %>% summarise(n_metoo0 = sum(metoo0), n_metoo1 = sum(metoo1), n_metoo_many = sum(metoo_many)) -> commitment_metoo


## Time's Up
cp_hash %>% left_join(cp_wc_tibble) %>% group_by(user, group) %>%
  summarise(timesup_freq = sum(timesup))

cp_hash %>% left_join(cp_wc_tibble) %>% group_by(user, group) %>%
  summarise(timesup_freq = sum(timesup)) %>% ungroup %>% 
  mutate(timesup0 = timesup_freq == 0, timesup1 = timesup_freq == 1, timesup_many = timesup_freq > 1)

cp_hash %>% left_join(cp_wc_tibble) %>% group_by(user, group) %>%
  summarise(timesup_freq = sum(timesup)) %>% ungroup %>% 
  mutate(timesup0 = timesup_freq == 0, timesup1 = timesup_freq == 1, timesup_many = timesup_freq > 1) %>% 
  group_by(group) %>% summarise(n_timesup0 = sum(timesup0), n_timesup1 = sum(timesup1), n_timesup_many = sum(timesup_many)) -> commitment_timesup

## Women's March
cp_hash %>% left_join(cp_wc_tibble) %>% group_by(user, group) %>%
  summarise(womensmarch_freq = sum(womensmarch))

cp_hash %>% left_join(cp_wc_tibble) %>% group_by(user, group) %>%
  summarise(womensmarch_freq = sum(womensmarch)) %>% ungroup %>% 
  mutate(womensmarch0 = womensmarch_freq == 0, womensmarch1 = womensmarch_freq == 1, womensmarch_many = womensmarch_freq > 1)

cp_hash %>% left_join(cp_wc_tibble) %>% group_by(user, group) %>%
  summarise(womensmarch_freq = sum(womensmarch)) %>% ungroup %>% 
  mutate(womensmarch0 = womensmarch_freq == 0, womensmarch1 = womensmarch_freq == 1, womensmarch_many = womensmarch_freq > 1) %>% 
  group_by(group) %>% summarise(n_womensmarch0 = sum(womensmarch0), n_womensmarch1 = sum(womensmarch1), n_womensmarch_many = sum(womensmarch_many)) -> commitment_wm


## Public attention (H3, H4, H4a)

articles <- readRDS('./articles.rds')

## Total public attention for MeToo in national newspapers
sum(str_count(articles$content, "Me Too")) #106
sum(str_count(articles$content, "metoo")) #49
sum(str_count(articles$content, "MeToo")) #1254
sum(str_count(articles$title, "Me Too")) #7
sum(str_count(articles$title, "metoo")) #0
sum(str_count(articles$title, "MeToo")) #127
#total:1543

## Public attention for MeToo in The New York Times
articles %>% select(source, title, content) %>% filter(articles$source == "The New York Times") -> articles_nyt_metoo
sum(str_count(articles_nyt_metoo$content, "Me Too")) #17
sum(str_count(articles_nyt_metoo$content, "metoo")) #33
sum(str_count(articles_nyt_metoo$content, "MeToo")) #757
sum(str_count(articles_nyt_metoo$title, "Me Too")) #1
sum(str_count(articles_nyt_metoo$title, "metoo")) #0
sum(str_count(articles_nyt_metoo$title, "MeToo")) #76
#total NYT = 884

## Public attention for MeToo in The Washington Post
articles %>% select(source, title, content) %>% filter(articles$source == "The Washington Post") -> articles_wp_metoo
sum(str_count(articles_wp_metoo$content, "Me Too")) #20 
sum(str_count(articles_wp_metoo$content, "metoo")) #6
sum(str_count(articles_wp_metoo$content, "MeToo")) #427 
sum(str_count(articles_wp_metoo$title, "Me Too")) #0
sum(str_count(articles_wp_metoo$title, "metoo")) #0
sum(str_count(articles_wp_metoo$title, "MeToo")) #44
#total WP = 497

## Public attention for MeToo in USA Today
articles %>% select(source, title, content) %>% filter(articles$source == "USA Today") -> articles_usa_metoo
sum(str_count(articles_usa_metoo$content, "Me Too")) #69
sum(str_count(articles_usa_metoo$content, "metoo")) #10
sum(str_count(articles_usa_metoo$content, "MeToo")) #70
sum(str_count(articles_usa_metoo$title, "Me Too")) #6
sum(str_count(articles_usa_metoo$title, "metoo")) #0
sum(str_count(articles_usa_metoo$title, "MeToo")) #7
#total USA Today = 162

## Total public attention for Time's Up in national newspapers
sum(str_count(articles$content, "Time's Up")) #465
sum(str_count(articles$content, "timesup")) #2
sum(str_count(articles$content, "TimesUp")) #34
sum(str_count(articles$title, "Time's Up")) #11
sum(str_count(articles$title, "timesup")) #0
sum(str_count(articles$title, "TimesUp")) #0
#total = 512

## Public attention for Time's Up in The New York Times
articles %>% select(source, title, content) %>% filter(articles$source == "The New York Times") -> articles_nyt_tu
sum(str_count(articles_nyt_tu$content, "Time's Up")) #311
sum(str_count(articles_nyt_tu$content, "timesup")) #1
sum(str_count(articles_nyt_tu$content, "TimesUp")) #20
sum(str_count(articles_nyt_tu$title, "Time's Up")) #7
sum(str_count(articles_nyt_tu$title, "timesup")) #0
sum(str_count(articles_nyt_tu$title, "TimesUp")) #0
#total NYT = 339

## Public attention for Time's Up in The Washington Post
articles %>% select(source, title, content) %>% filter(articles$source == "The Washington Post") -> articles_wp_tu
sum(str_count(articles_wp_tu$content, "Time's Up")) #67
sum(str_count(articles_wp_tu$content, "timesup")) #0
sum(str_count(articles_wp_tu$content, "TimesUp")) #12
sum(str_count(articles_wp_tu$title, "Time's Up")) #0
sum(str_count(articles_wp_tu$title, "timesup")) #0
sum(str_count(articles_wp_tu$title, "TimesUp")) #0
#total WP = 79

## Public attention for Time's Up in USA Today
articles %>% select(source, title, content) %>% filter(articles$source == "USA Today") -> articles_usa_tu
sum(str_count(articles_usa_tu$content, "Time's Up")) #87
sum(str_count(articles_usa_tu$content, "timesup")) #1
sum(str_count(articles_usa_tu$content, "TimesUp")) #2
sum(str_count(articles_usa_tu$title,"Time's Up")) #4
sum(str_count(articles_usa_tu$title, "timesup")) #0
sum(str_count(articles_usa_tu$title, "TimesUp")) #0
#total USA Today = 94

## Total public attention for Women's March in national newspapers
sum(str_count(articles$content, "Women's March")) #806
sum(str_count(articles$content, "womensmarch")) #7
sum(str_count(articles$content, "WomensMarch")) #2
sum(str_count(articles$title, "Women's March")) #23
sum(str_count(articles$title, "womensmarch")) #0
sum(str_count(articles$title, "WomensMarch")) #0
#total WM = 838

## Public attention for Women's March in The New York Times
articles %>% select(source, title, content) %>% filter(articles$source == "The New York Times") -> articles_nyt_wm
sum(str_count(articles_nyt_wm$content, "Women's March")) #316
sum(str_count(articles_nyt_wm$content, "womensmarch")) #6
sum(str_count(articles_nyt_wm$content, "WomensMarch")) #2
sum(str_count(articles_nyt_wm$title, "Women's March")) #7
sum(str_count(articles_nyt_wm$title,  "womensmarch")) #0
sum(str_count(articles_nyt_wm$title, "WomensMarch")) #0
#total NYT = 339

## Public attention for Women's March in The Washington Post
articles %>% select(source, title, content) %>% filter(articles$source == "The Washington Post") -> articles_wp_wm
sum(str_count(articles_wp_wm$content,  "Women's March")) #419
sum(str_count(articles_wp_wm$content,  "womensmarch")) #1
sum(str_count(articles_wp_wm$content, "WomensMarch")) #0
sum(str_count(articles_wp_wm$title, "Women's March")) #14
sum(str_count(articles_wp_wm$title,  "womensmarch")) #0
sum(str_count(articles_wp_wm$title, "WomensMarch")) #0
#total WP = 434

## Public attention for Women's March in USA Today
articles %>% select(source, title, content) %>% filter(articles$source == "USA Today") -> articles_usa_wm
sum(str_count(articles_usa_wm$content, "Women's March")) #71
sum(str_count(articles_usa_wm$content, "womensmarch")) #0
sum(str_count(articles_usa_wm$content, "WomensMarch")) #0
sum(str_count(articles_usa_wm$title,"Women's March")) #2
sum(str_count(articles_usa_wm$title, "womensmarch")) #0
sum(str_count(articles_usa_wm$title, "WomensMarch")) #0
#total USA Today = 73


## Public attention in regional newspapers (H4a)
regional <- readRDS("./Regional_articles_lexis.rds")
regional %>% mutate(lower_text = tolower(content)) -> regional
                    
## Total public attention for MeToo in regional newspapers
sum(str_count(regional$lower_text, "me too")) #33
sum(str_count(regional$lower_text, "metoo")) #230
sum(str_count(regional$title, "Me Too")) #2
sum(str_count(regional$title, "metoo")) #2
sum(str_count(regional$title, "MeToo")) #17
#total MeToo: 284

## Public attention for MeToo in the Charleston Gazette
regional_charleston <- regional %>% select(source, title, lower_text) %>% filter(source == "Charleston Gazette")
sum(str_count(regional_charleston$lower_text, "me too")) #12
sum(str_count(regional_charleston$lower_text, "metoo")) #81
sum(str_count(regional_charleston$title, "Me Too")) #1
sum(str_count(regional_charleston$title, "metoo")) #1
sum(str_count(regional_charleston$title, "MeToo")) #6
#total Charleston Gazette: 101

## Public attention for MeToo in the Chicago Daily Herald
regional_chicago <- regional %>% select(source, title, lower_text) %>% filter(source == "Daily Herald")
sum(str_count(regional_chicago$lower_text, "me too")) #21
sum(str_count(regional_chicago$lower_text, "metoo")) #149
sum(str_count(regional_chicago$title, "Me Too")) #1
sum(str_count(regional_chicago$title, "metoo")) #1
sum(str_count(regional_chicago$title, "MeToo")) #11
#total Chicago Daily Herald: 183

## Total public attention for Time's Up in regional newspapers
sum(str_count(regional$lower_text, "time's up")) #49
sum(str_count(regional$lower_text, "timesup")) #5
sum(str_count(regional$title, "Time's Up")) #0
sum(str_count(regional$title, "timesup")) #0
sum(str_count(regional$title, "TimesUp")) #0
#total Time's Up: 54

## Public attention for Time's Up in the Charleston Gazette
sum(str_count(regional_charleston$lower_text, "time's up")) #13
sum(str_count(regional_charleston$lower_text, "timesup")) #0
sum(str_count(regional_charleston$title, "Time's Up")) #0
sum(str_count(regional_charleston$title, "timesup")) #0
sum(str_count(regional_charleston$title, "TimesUp")) #0
#total Charleston Gazette: 13

## Public attention for Time's Up in the Chicago Daily Herald
sum(str_count(regional_chicago$lower_text, "time's up")) #36
sum(str_count(regional_chicago$lower_text, "timesup")) #5
sum(str_count(regional_chicago$title, "Time's Up")) #0
sum(str_count(regional_chicago$title, "timesup")) #0
sum(str_count(regional_chicago$title, "TimesUp")) #0
#total Chicago Daily Herald: 41

## Total public attention for Women's March in regional newspapers
sum(str_count(regional$lower_text, "women's march")) #488
sum(str_count(regional$lower_text, "womensmarch")) #5
sum(str_count(regional$title, "Women's March")) #12
sum(str_count(regional$title, "womensmarch")) #0
sum(str_count(regional$title, "WomensMarch")) #0
#total Women's March: 505

## Public attention for Women's March in the Charleston Gazette
sum(str_count(regional_charleston$lower_text, "women's march")) #104
sum(str_count(regional_charleston$lower_text, "womensmarch")) #2
sum(str_count(regional_charleston$title, "Women's March")) #3
sum(str_count(regional_charleston$title, "womensmarch")) #0
sum(str_count(regional_charleston$title, "WomensMarch")) #0
#total Charleston Gazette: 109

## Public attention for Women's March in the Chicago Daily Herald
sum(str_count(regional_chicago$lower_text, "women's march")) #384
sum(str_count(regional_chicago$lower_text, "womensmarch")) #3
sum(str_count(regional_chicago$title, "Women's March")) #9
sum(str_count(regional_chicago$title, "womensmarch")) #0
sum(str_count(regional_chicago$title, "WomensMarch")) #0
#total Chicago Daily Herald: 396


## Attention of political elites (H4b)
require(rtweet)

cp_data %>% select(user, fullname, text) %>% filter(user == "BarackObama") #no result
cp_data %>% select(user, fullname, text) %>% filter(user == "realDonaldTrump") #no result
cp_data %>% select(user, fullname, timestamp, likes, replies, retweets, text) %>% filter(user == "HillaryClinton") -> hillaryclinton_tweets##1 result
which(grepl("HillaryClinton", cp_data$user)) ##looking up the row of the tweet in the data
cp_data[1344222,8] ##getting the tweet from Hillary Clinton

cp_data %>% select(user, fullname, timestamp, likes, replies, retweets, text) %>% filter(user == "MichelleObama") -> michelleobama_tweets ##3 tweets
which(grepl("MichelleObama", cp_data$user)) ##rows: 432914, 1454153, 2055053 (2x das selbe und 1x MichelleObamaPr)
cp_data[432914]

cp_data %>% select(user, fullname, text) %>% filter(user == "FLOTUS")
cp_data %>% select(user, fullname, text) %>% filter(user == "BillClinton")
cp_data %>% select(user, fullname, text) %>% filter(user == "BernieSanders")
cp_data %>% select(user, fullname, timestamp, likes, replies, retweets, text) %>% filter(user == "AOC") -> aoc_tweets ##11 results
which(grepl("Alexandria Ocasio-Cortez", cp_data$fullname))
cp_data[530430,8]

cp_data %>% select(user, fullname, text) %>% filter(user == "CoryBooker")
cp_data %>% select(user, fullname, text) %>% filter(user == "marcorubio")
cp_data %>% select(user, fullname, text) %>% filter(user == "mike_pence")
cp_data %>% select(user, fullname, text) %>% filter(user == "SpeakerRyan")
cp_data %>% select(user, fullname, text) %>% filter(user == "JoeBiden")
cp_data %>% select(user, fullname, text) %>% filter(user == "tedcruz")
cp_data %>% select(user, fullname, text) %>% filter(user == "johnkerry")
cp_data %>% select(user, fullname, text) %>% filter(user == "algore")
cp_data %>% select(user, fullname,timestamp, likes, replies, retweets, text) %>% filter(user == "KamalaHarris") -> kamalaharris_tweets
which(grepl("Kamala Harris", cp_data$fullname))
cp_data[5737,1]

cp_data %>% select(user, fullname,timestamp, likes, replies, retweets, text) %>% filter(user == "SpeakerPelosi") -> pelosi_tweets
which(grepl("SpeakerPelosi", cp_data$user))
cp_data$text[679619]
cp_data[1996171,1]
pelosi_tweets %>% distinct(text, .keep_all = TRUE) -> pelosi_tweets

cp_data %>% select(user, fullname, text) %>% filter(user == "randpaul")

## creating politics tweets data
politics <- rbind(kamalaharris_tweets, pelosi_tweets, aoc_tweets, hillaryclinton_tweets, michelleobama_tweets)
politics %>% distinct(text, .keep_all = TRUE) -> politics_twitter

politics_twitter %>% mutate(lower_text = tolower(text), metoo = str_detect(lower_text, 'metoo'), 
                 timesup = str_detect(lower_text, 'timesup'), 
                 womensmarch = str_detect(lower_text, 'womensmarch')) -> politics_movement
politics_movement = politics_movement[-c(22),]

politics_movement %>% group_by(metoo, timesup, womensmarch) %>% tally

mean(politics_movement$likes) #4833,92
mean(politics_movement$replies) # 193,16
mean(politics_movement$retweets) #966,8

politics_corpus <- corpus(politics_movement$lower_text)
docvars(politics_corpus, "user") <- politics_movement$user
docvars(politics_corpus, "retweets") <- politics_movement$retweets
docvars(politics_corpus, "timestamp") <- politics_movement$timestamp
summary(politics_corpus)

politics_dfm <- dfm(politics_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, remove_symbols = TRUE,remove = stopwords('en'))
                                   
## get frequency in all politics tweets
textstat_keyness(politics_dfm)
features_politics_dfm <- textstat_frequency(politics_dfm, n = 25)

## visualize
features_politics_dfm$feature <- with(features_politics_dfm, reorder(feature, -frequency))

ggplot(features_politics_dfm, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## show which user used which movements
politics_movement %>% group_by(user) %>% summarise(metoo = any(metoo), timesup = any(timesup), womensmarch = any(womensmarch)) %>% 
  select(user, metoo, timesup, womensmarch)

## Hashtag combination in tweets from political elite
politics_movement %>% group_by(metoo, timesup, womensmarch) %>% tally



##### H5 - Granger's Causality #####

#filter news_mentions of the movements
news_articles <- readRDS("articles.rds")
news_mentions_metoo <- readRDS("news_mentions_metoo.RDS")

news_articles %>% mutate(lower_text = tolower(content), timesup = str_detect(lower_text, '#timesup')) %>% 
  filter(str_detect(lower_text, "timesup")) -> news_mentions_timesup

news_articles %>% mutate(lower_text = tolower(content), womensmarch = str_detect(lower_text, '#womensmarch')) %>% 
  filter(str_detect(lower_text, "womensmarch")) -> news_mentions_womensmarch


require(lubridate)
## change the date to English first

news_mentions_metoo %>% count(date) %>% mutate(date = str_replace(date, "März", "March")) %>% 
  mutate(date = str_replace(date, "Dezem", "Decem")) %>% mutate(date = str_replace(date, "Okto", "Octo")) %>%  
  mutate(date = dmy(date)) %>% arrange(date) -> news_mention_metoo_ts

news_mentions_timesup %>% count(date) %>% mutate(date = str_replace(date, "März", "March")) %>% 
  mutate(date = str_replace(date, "Dezem", "Decem")) %>% mutate(date = str_replace(date, "Okto", "Octo")) %>%  
  mutate(date = dmy(date)) %>% arrange(date) -> news_mention_timesup_ts

news_mentions_womensmarch %>% count(date) %>% mutate(date = str_replace(date, "März", "March")) %>% 
  mutate(date = str_replace(date, "Dezem", "Decem")) %>% mutate(date = str_replace(date, "Okto", "Octo")) %>%  
  mutate(date = dmy(date)) %>% arrange(date) -> news_mention_womensmarch_ts

devtools::install_github("chainsawriot/nucommit")
install.packages("zoo")
library(nucommit)
require(zoo)

## Unity & Commitment MeToo
## Unity:
cp_hash %>% select(user, group, timestamp, lower_text, metoo) %>% filter(metoo == "TRUE") %>% calculate_unity() %>% mutate(date = as_date(created_at)) %>% select(date, unity) -> unity_ts_metoo

## selecting only the time range match your news mentions
unity_ts_metoo %>% filter(date >= min(news_mention_metoo_ts$date) & date <= max(news_mention_metoo_ts$date)) -> unity_ts_metoo

## check if there is missing date in unity_ts_metoo
all(unity_ts_metoo$date %in% seq(min(news_mention_metoo_ts$date), max(news_mention_metoo_ts$date), by = 'day'))

require(zoo)
install.packages("zoo")

unity_metoo_zoo <- zoo(unity_ts_metoo$unity, unity_ts_metoo$date)
newsmention_metoo_zoo <- zoo(news_mention_metoo_ts$n, news_mention_metoo_ts$date)

binded_ts_unity_metoo <- merge(unity_metoo_zoo, newsmention_metoo_zoo)

## NA in news mention should be zero mention (No news about it.)

binded_ts_unity_metoo$newsmention_metoo_zoo[is.na(binded_ts_unity_metoo$newsmention_metoo_zoo)] <- 0

require(lmtest)

## Grangertest for lag 1, lag 2 and lag 4 nemsmentions -> unity
grangertest(binded_ts_unity_metoo$newsmention_metoo_zoo, binded_ts_unity_metoo$unity_metoo_zoo, order = 1)
grangertest(binded_ts_unity_metoo$newsmention_metoo_zoo, binded_ts_unity_metoo$unity_metoo_zoo, order = 2)
grangertest(binded_ts_unity_metoo$newsmention_metoo_zoo, binded_ts_unity_metoo$unity_metoo_zoo, order = 4)

## Grangertest for lag 1, lag 2 and lag 4 unity -> newsmentions
grangertest(binded_ts_unity_metoo$unity_metoo_zoo, binded_ts_unity_metoo$newsmention_metoo_zoo, order = 1)
grangertest(binded_ts_unity_metoo$unity_metoo_zoo, binded_ts_unity_metoo$newsmention_metoo_zoo, order = 2)
grangertest(binded_ts_unity_metoo$unity_metoo_zoo, binded_ts_unity_metoo$newsmention_metoo_zoo, order = 4)

plot(binded_ts_unity_metoo)


## Commitment:
cp_hash %>% select(user, timestamp, text, metoo) %>% filter(metoo == "TRUE") %>% calculate_commitment() %>% mutate(date = as_date(created_at)) %>% select(date, commitment) -> commitment_ts_metoo
commitment_ts_metoo %>% filter(date >= min(news_mention_metoo_ts$date) & date <= max(news_mention_metoo_ts$date)) -> commitment_ts_metoo

all(commitment_ts_metoo$date %in% seq(min(news_mention_metoo_ts$date), max(news_mention_metoo_ts$date), by = 'day'))

commitment_metoo_zoo <- zoo(commitment_ts_metoo$commitment, commitment_ts_metoo$date)
newsmention_metoo_zoo <- zoo(news_mention_metoo_ts$n, news_mention_metoo_ts$date)

binded_ts_commitment_metoo <- merge(commitment_metoo_zoo, newsmention_metoo_zoo)
binded_ts_commitment_metoo$newsmention_metoo_zoo[is.na(binded_ts_commitment_metoo$newsmention_metoo_comm_zoo)] <- 0

## Grangertest for lag 1, lag 2 and lag 4 nemsmentions -> commitment
grangertest(binded_ts_commitment_metoo$newsmention_metoo_zoo, binded_ts_commitment_metoo$commitment_metoo_zoo, order = 1)
grangertest(binded_ts_commitment_metoo$newsmention_metoo_zoo, binded_ts_commitment_metoo$commitment_metoo_zoo, order = 2)
grangertest(binded_ts_commitment_metoo$newsmention_metoo_zoo, binded_ts_commitment_metoo$commitment_metoo_zoo, order = 4)

## Grangertest for lag 1, lag 2 and lag 4 commitment -> newsmentions
grangertest(binded_ts_commitment_metoo$commitment_metoo_zoo, binded_ts_commitment_metoo$newsmention_metoo_zoo, order = 1)
grangertest(binded_ts_commitment_metoo$commitment_metoo_zoo, binded_ts_commitment_metoo$newsmention_metoo_zoo, order = 2)
grangertest(binded_ts_commitment_metoo$commitment_metoo_zoo, binded_ts_commitment_metoo$newsmention_metoo_zoo, order = 4)

plot(binded_ts_commitment_metoo)


## unity & commitment Time's Up
## Unity:
cp_hash %>% select(user, timestamp, text, timesup) %>% filter(timesup == "TRUE") %>% calculate_unity() %>% mutate(date = as_date(created_at)) %>% select(date, unity) -> unity_ts_timesup
unity_ts_timesup %>% filter(date >= min(news_mention_timesup_ts$date) & date <= max(news_mention_timesup_ts$date)) -> unity_ts_timesup

all(unity_ts_timesup$date %in% seq(min(news_mention_timesup_ts$date), max(news_mention_timesup_ts$date), by = 'day'))

unity_timesup_zoo <- zoo(unity_ts_timesup$unity, unity_ts_timesup$date)
newsmention_timesup_zoo <- zoo(news_mention_timesup_ts$n, news_mention_timesup_ts$date)

binded_ts_unity_timesup <- merge(unity_timesup_zoo, newsmention_timesup_zoo)
binded_ts_unity_timesup$newsmention_timesup_zoo[is.na(binded_ts_unity_timesup$newsmention_timesup_zoo)] <- 0

## Grangertest for lag 1, lag 2 and lag 4 nemsmentions -> unity
grangertest(binded_ts_unity_timesup$newsmention_timesup_zoo, binded_ts_unity_timesup$unity_timesup_zoo, order = 1)
grangertest(binded_ts_unity_timesup$newsmention_timesup_zoo, binded_ts_unity_timesup$unity_timesup_zoo, order = 2)
grangertest(binded_ts_unity_timesup$newsmention_timesup_zoo, binded_ts_unity_timesup$unity_timesup_zoo, order = 4)

## Grangertest for lag 1, lag 2 and lag 4 unity -> newsmentions
grangertest(binded_ts_unity_timesup$unity_timesup_zoo, binded_ts_unity_timesup$newsmention_timesup_zoo, order = 1)
grangertest(binded_ts_unity_timesup$unity_timesup_zoo, binded_ts_unity_timesup$newsmention_timesup_zoo, order = 2)
grangertest(binded_ts_unity_timesup$unity_timesup_zoo, binded_ts_unity_timesup$newsmention_timesup_zoo, order = 4)

plot(binded_ts_unity_timesup)


##Commitment:
cp_hash %>% select(user, timestamp, text, timesup) %>% filter(timesup == "TRUE") %>% calculate_commitment() %>% mutate(date = as_date(created_at)) %>% select(date, commitment) -> commitment_ts_timesup
commitment_ts_timesup %>% filter(date >= min(news_mention_timesup_ts$date) & date <= max(news_mention_timesup_ts$date)) -> commitment_ts_timesup

all(commitment_ts_timesup$date %in% seq(min(news_mention_timesup_ts$date), max(news_mention_timesup_ts$date), by = 'day'))

commitment_timesup_zoo <- zoo(commitment_ts_timesup$commitment, commitment_ts_timesup$date)
newsmention_timesup_zoo <- zoo(news_mention_timesup_ts$n, news_mention_timesup_ts$date)

binded_ts_commitment_timesup <- merge(commitment_timesup_zoo, newsmention_timesup_zoo)
binded_ts_commitment_timesup$newsmention_timesup_zoo[is.na(binded_ts_commitment_timesup$newsmention_timesup_zoo)] <- 0

## Grangertest for lag 1, lag 2 and lag 4 newsmentions -> commitment
grangertest(binded_ts_commitment_timesup$newsmention_timesup_zoo, binded_ts_commitment_timesup$commitment_timesup_zoo, order = 2)
grangertest(binded_ts_commitment_timesup$newsmention_timesup_zoo, binded_ts_commitment_timesup$commitment_timesup_zoo, order = 4)
grangertest(binded_ts_commitment_timesup$newsmention_timesup_zoo, binded_ts_commitment_timesup$commitment_timesup_zoo, order = 1)

## Grangertest for lag 1, lag 2 and lag 4 commitment -> newsmentions
grangertest(binded_ts_commitment_timesup$commitment_timesup_zoo, binded_ts_commitment_timesup$newsmention_timesup_zoo, order = 2)
grangertest(binded_ts_commitment_timesup$commitment_timesup_zoo, binded_ts_commitment_timesup$newsmention_timesup_zoo, order = 4) 
grangertest(binded_ts_commitment_timesup$commitment_timesup_zoo, binded_ts_commitment_timesup$newsmention_timesup_zoo, order = 1) 

plot(binded_ts_commitment_timesup)


##Unity & Commitment Women's March
## Unity:
cp_hash %>% select(user, timestamp, text, womensmarch) %>% filter(womensmarch == "TRUE") %>% calculate_unity() %>% mutate(date = as_date(created_at)) %>% select(date, unity) -> unity_ts_womensmarch
unity_ts_womensmarch %>% filter(date >= min(news_mention_womensmarch_ts$date) & date <= max(news_mention_womensmarch_ts$date)) -> unity_ts_womensmarch

all(unity_ts_womensmarch$date %in% seq(min(news_mention_womensmarch_ts$date), max(news_mention_womensmarch_ts$date), by = 'day'))

unity_womensmarch_zoo <- zoo(unity_ts_womensmarch$unity, unity_ts_womensmarch$date)
newsmention_womensmarch_zoo <- zoo(news_mention_womensmarch_ts$n, news_mention_womensmarch_ts$date)

binded_ts_unity_womensmarch <- merge(unity_womensmarch_zoo, newsmention_womensmarch_zoo)
binded_ts_unity_womensmarch$newsmention_womensmarch_zoo[is.na(binded_ts_unity_womensmarch$newsmention_womensmarch_zoo)] <- 0

## Grangertest for lag 1, lag 2 and lag 4 newsmentions -> unity
grangertest(binded_ts_unity_womensmarch$newsmention_womensmarch_zoo, binded_ts_unity_womensmarch$unity_womensmarch_zoo, order = 1)
grangertest(binded_ts_unity_womensmarch$newsmention_womensmarch_zoo, binded_ts_unity_womensmarch$unity_womensmarch_zoo, order = 2)
grangertest(binded_ts_unity_womensmarch$newsmention_womensmarch_zoo, binded_ts_unity_womensmarch$unity_womensmarch_zoo, order = 4)

## Grangertest for lag 1, lag 2 and lag 4 unity -> newsmentions
grangertest(binded_ts_unity_womensmarch$unity_womensmarch_zoo, binded_ts_unity_womensmarch$newsmention_womensmarch_zoo, order = 1)
grangertest(binded_ts_unity_womensmarch$unity_womensmarch_zoo, binded_ts_unity_womensmarch$newsmention_womensmarch_zoo, order = 2)
grangertest(binded_ts_unity_womensmarch$unity_womensmarch_zoo, binded_ts_unity_womensmarch$newsmention_womensmarch_zoo, order = 4)

plot(binded_ts_unity_womensmarch)


##Commitment:
cp_hash %>% select(user, timestamp, text, womensmarch) %>% filter(womensmarch == "TRUE") %>% calculate_commitment() %>% mutate(date = as_date(created_at)) %>% select(date, commitment) -> commitment_ts_womensmarch
commitment_ts_womensmarch %>% filter(date >= min(news_mention_womensmarch_ts$date) & date <= max(news_mention_womensmarch_ts$date)) -> commitment_ts_womensmarch

all(commitment_ts_womensmarch$date %in% seq(min(news_mention_womensmarch_ts$date), max(news_mention_womensmarch_ts$date), by = 'day'))

commitment_womensmarch_zoo <- zoo(commitment_ts_womensmarch$commitment, commitment_ts_womensmarch$date)
newsmention_womensmarch_zoo <- zoo(news_mention_womensmarch_ts$n, news_mention_womensmarch_ts$date)

binded_ts_commitment_womensmarch <- merge(commitment_womensmarch_zoo, newsmention_womensmarch_zoo)
binded_ts_commitment_womensmarch$newsmention_womensmarch_zoo[is.na(binded_ts_commitment_womensmarch$newsmention_womensmarch_zoo)] <- 0

## Grangertest for lag 1, lag 2 and lag 4 newsmentions -> commitment
grangertest(binded_ts_commitment_womensmarch$newsmention_womensmarch_zoo, binded_ts_commitment_womensmarch$commitment_womensmarch_zoo, order = 1)
grangertest(binded_ts_commitment_womensmarch$newsmention_womensmarch_zoo, binded_ts_commitment_womensmarch$commitment_womensmarch_zoo, order = 2)
grangertest(binded_ts_commitment_womensmarch$newsmention_womensmarch_zoo, binded_ts_commitment_womensmarch$commitment_womensmarch_zoo, order = 4)

## Grangertest for lag 1, lag 2 and lag 4 commitment -> newsmentions
grangertest(binded_ts_commitment_womensmarch$commitment_womensmarch_zoo, binded_ts_commitment_womensmarch$newsmention_womensmarch_zoo, order = 1)
grangertest(binded_ts_commitment_womensmarch$commitment_womensmarch_zoo, binded_ts_commitment_womensmarch$newsmention_womensmarch_zoo, order = 2)
grangertest(binded_ts_commitment_womensmarch$commitment_womensmarch_zoo, binded_ts_commitment_womensmarch$newsmention_womensmarch_zoo, order = 4) 

plot(binded_ts_commitment_womensmarch)


##### Descriptive statistics #####
cp_data$id <- 1:nrow(cp_data)
ddply(cp_data,~user,summarise,number_of_distinct_orders=length(unique(id))) %>% arrange(desc(number_of_distinct_orders)) -> cp_data_uu
