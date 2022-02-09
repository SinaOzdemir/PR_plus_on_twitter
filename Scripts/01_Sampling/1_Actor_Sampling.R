#Account sampling:

# packages: ---------------------------------------------------------------

packs = c("readxl","xlsx","tidyverse","moments")
lapply(packs,library,character.only =T)


# SaveDIRs: ---------------------------------------------------------------

graphDIR <- paste0(getwd(),"Graphs/")

dataDIR <- paste0(getwd(),"Data/")

# Data: -------------------------------------------------------------------


id = read.xlsx(file = paste0(dataDIR,"ID_variables_v2.xlsx"),sheetIndex = 1,encoding = "UTF-8")

non_pol <- filter(id, !Actor_type%in%c("MEP","Party group","Political party","Head of State"))

# Sample MEPS: ------------------------------------------------------------

mep <- filter(id, Actor_type %in% "MEP")

mep$Actor_EU_party = gsub("Confederal ","",mep$Actor_EU_party)
mep$Actor_EU_party = gsub("/","-", mep$Actor_EU_party)

countryMEP = unique(as.character(mep$Actor_nationality))
eppartyMEP = unique(as.character(mep$Actor_EU_party))

stratifiedMEPS = tibble()


for (party in eppartyMEP) {
  
  p_MEP = filter(mep, Actor_EU_party == party)
  
  #uncomment to get the barplots of frequencies
  
  # party_cntry = p_MEP %>% 
  #   group_by(Actor_nationality) %>% 
  #   tally(sort = T) %>% 
  #   ggplot(aes(x = reorder(Actor_nationality,n), y = n))+
  #   geom_bar(aes(fill = n),position = "dodge",stat = "identity")+
  #   coord_flip()+labs(x = "Country",title = paste0("composition of ",party," by country"))
  # 
  # ggsave(filename = paste0(party,".jpeg"),plot = party_cntry,path = paste0(graphDIR,"Party compositon"))
  # 
  for (cntry in countryMEP) {
    
    c_MEP = filter(p_MEP, Actor_nationality == cntry)
    
    # cntry_flwr = c_MEP %>% 
    #   arrange(followers_count) %>% 
    #   mutate(Actor_name = factor(Actor_name,levels = Actor_name)) %>% 
    #   ggplot(aes(x = Actor_name, y = followers_count))+
    #   geom_bar(aes(fill = followers_count),position = "dodge",stat = "identity")+
    #   coord_flip()+labs(title = paste0(party), subtitle = paste0(cntry),x ="", y = "follower count")
    # 
    # ggsave(filename = paste0(party,"_",cntry,".jpeg"),plot = cntry_flwr,path = paste0(graphDIR,"Follower distribution"))
    # 
    flwr_quantile = quantile(c_MEP$followers_count)
    
    
    impMEPS = filter(c_MEP, followers_count >= flwr_quantile[["75%"]])
    #MEPS are stratified by party,country, and follower count, then most prominent 25% (on twitter) selected
    stratifiedMEPS = rbind(stratifiedMEPS,impMEPS)
    
  }
}

stratifiedMEPS %>%
  top_n(20,followers_count)%>% 
  mutate(Actor_name = factor(Actor_name,levels = Actor_name)) %>% 
  ggplot(aes(x = reorder(Actor_name,followers_count), y = followers_count))+
  geom_bar(aes(fill = followers_count),position = "dodge",stat = "identity")+
  coord_flip()+labs(title = "MEP final sample",x ="", y = "follower count") 

non_mep <- filter(id, Actor_type != "MEP")

sampledAccounts <- rbind(non_mep,stratifiedMEPS)

saveRDS(sampledAccounts,file = paste0(dataDIR,"sampledAccounts.RDS"))

write.xlsx(sampledAccounts,file = paste0(dataDIR,"sampledAccounts.xlsx"),showNA = F)


# Trim the tweets by stratified account -----------------------------------

tweets <- list.files(path = paste0(dataDIR,"Actor Data/"),pattern = "*.RDS",full.names = T)

tweets <- lapply(tweets, readRDS)

tweets <- do.call("rbind",tweets)

tweets_id <- select(tweets, user_id,screen_name,name)

tweets_id <- tweets_id[!duplicated(tweets_id$user_id),]

trimmed_id_semi <- semi_join(tweets_id,sampledAccounts, by = "user_id")

trimmed_tweets <- semi_join(tweets,trimmed_id_semi, by = "user_id")


saveRDS(object = trimmed_tweets,file = paste0(dataDIR,"trimmed_tweets.RDS"))

trimmed_pure_tweets <- select(trimmed_tweets,user_id, screen_name,status_id,created_at,date,text)

write.csv(x = trimmed_pure_tweets,file = paste0(dataDIR,"trimmed_tweets_nometa.csv"),na = "NA",quote = T,fileEncoding = "UTF-8")


sampledAccounts_final <- semi_join(sampledAccounts,tweets_id, by = "user_id")

sampledAccounts_final <- sampledAccounts_final[!duplicated(sampledAccounts_final$user_id),]

saveRDS(sampledAccounts_final,file = paste0(dataDIR,"sampledAccounts_final_meta.RDS"))

write.xlsx(sampledAccounts_final,file = paste0(dataDIR,"sampledAccounts_final_meta.xlsx"),showNA = F)


# Non_political sample ----------------------------------------------------
amjj<-list.files(path = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/phd data/Phd_data_collection/Scripts/extended_list_collection/Actor_tweets",pattern = "*.RDS",full.names = T)

amjj<- amjj[170:282]

amjj_dta<- lapply(amjj,FUN = function(x){readRDS(x)})

amjj_dta<-do.call("rbind",amjj_dta)

amjj_dta_npl <- filter(amjj_dta,user_id%in%non_pol$user_id)
amjj_dta_npl_nrt<-filter(amjj_dta_npl,is_retweet == F)

twt<- readRDS(file = paste0(dataDIR,"tweet_data.RDS"))

twt_no_political <- filter(twt, user_id %in% non_pol$user_id)

twt_np_rt<- filter(twt_no_political, is_retweet == T)

twt_no_nrt<- filter(twt_no_political, is_retweet == F)

twt_no_nrt<- twt_no_nrt[!duplicated(twt_no_nrt$status_id),]

twt_npl_full_nrt<- rbind(twt_no_nrt,amjj_dta_npl_nrt)
saveRDS(twt_npl_full_nrt,file = paste0(getwd(),"/Paper_1_data/full_npl_nrt_1219_0720.RDS"))

twt_no_nrt_smry <- twt_npl_full_nrt %>% group_by(screen_name) %>% summarise(t_count = n(),
                                                                      m_retweet = round(mean(retweet_count)),
                                                                      m_fav = round(mean(favorite_count)),
                                                                      max_retweet = max(retweet_count),
                                                                      max_fav = max(favorite_count))

np_nrt<- select(.data = twt_no_nrt,screen_name, followers_count,friends_count)
np_nrt <- np_nrt[!duplicated(np_nrt$screen_name),]


twt_no_nrt_smry<- left_join(x = twt_no_nrt_smry, y = np_nrt, by = "screen_name")
twt_no_nrt_smry$wghtd_retweet <- (twt_no_nrt_smry$m_retweet/twt_no_nrt_smry$followers_count)*1000

twt_no_nrt_smry$wghtd_fav <- (twt_no_nrt_smry$m_fav/twt_no_nrt_smry$followers_count)*1000

tweet_count_np = ggplot(data = twt_no_nrt_smry,aes(x = reorder(screen_name,t_count), y = t_count))+
  geom_bar(aes(fill = t_count), stat= "identity", position = "dodge")+
  coord_flip()+
  theme(axis.text.x = element_text(size = 9 ,hjust = 1))+labs(x= "Screen Name", y = "Tweet Count",title = "Tweet count of EU executive branch",subtitle = "from:01/12/2019\nuntill30/07/2020")


follower_count_np = ggplot(data = twt_no_nrt_smry,aes(x = reorder(screen_name,followers_count), y = followers_count))+
  geom_bar(aes(fill = followers_count), stat= "identity", position = "dodge")+
  coord_flip()+
  theme(axis.text.x = element_text(size = 9 ,hjust = 1))+labs(x= "Screen Name", y = "Followers Count",title = "Followers count of EU executive branch",subtitle = "from:01/12/2019\nuntill30/07/2020")

retweet_np = ggplot(data = twt_no_nrt_smry,aes(x = reorder(screen_name,m_retweet), y = m_retweet))+
  geom_bar(aes(fill = m_retweet), stat= "identity", position = "dodge")+
  coord_flip()+
  theme(axis.text.x = element_text(size = 9 ,hjust = 1))+labs(x= "Screen Name", y = "Weighted Average of Retweets",title = "Weighted average retweet count of EU executive branch",subtitle = "from:01/12/2019\nuntill30/07/2020")

retweet_max = ggplot(data = twt_no_nrt_smry,aes(x = reorder(screen_name,max_retweet), y = max_retweet))+
  geom_bar(aes(fill = max_retweet), stat= "identity", position = "dodge")+
  coord_flip()+
  theme(axis.text.x = element_text(size = 9 ,hjust = 1))+labs(x= "Screen Name", y = "Maximum Retweet",title = "Maximum retweet count of EU executive branch",subtitle = "from:01/12/2019\nuntill30/07/2020")

fav_np = ggplot(data = twt_no_nrt_smry,aes(x = reorder(screen_name,m_fav), y = m_fav))+
  geom_bar(aes(fill = m_fav), stat= "identity", position = "dodge")+
  coord_flip()+
  theme(axis.text.x = element_text(size = 9 ,hjust = 1))+labs(x= "Screen Name", y = "Average Favorite",title = "Average favorite count of EU executive branch",subtitle = "from:01/12/2019\nuntill30/07/2020")


weighted_retweet_np = ggplot(data = twt_no_nrt_smry,aes(x = reorder(screen_name,wghtd_retweet), y = wghtd_retweet))+
  geom_bar(aes(fill = wghtd_retweet), stat= "identity", position = "dodge")+
  coord_flip()+
  theme(axis.text.x = element_text(size = 9 ,hjust = 1))+labs(x= "Screen Name", y = "Weighted Average of Retweets",title = "Weighted average retweet count of EU executive branch",subtitle = "from:01/12/2019\nuntill30/07/2020\nweighting = (avg_retweet/followercount)*1000")

tweet_time <- select(twt_npl_full_nrt, screen_name,status_id,created_at,text)

tweet_time$day<- lubridate::floor_date(x = tweet_time$created_at,unit = "day")

tweet_time$week<- lubridate::floor_date(x = tweet_time$created_at,unit = "week")

tweet_time$month<- lubridate::floor_date(x = tweet_time$created_at,unit = "month")

tweet_time_day<- tweet_time %>% group_by(day) %>% summarise(t_count = n(),.groups = "keep") %>%  ggplot(aes(x = reorder(day,t_count),y = t_count))+
  geom_bar(aes(fill= t_count),stat = "identity",position = "dodge")+
  coord_flip()+theme(axis.text.x = element_text(angle = 45))+
  labs(x = "days",y = "tweet count",title = "Tweet count of Executive branch by day",subtitle = "N of Account = 119\ntfrom: 01/12/19\ntill: 30/07/20")

tweet_time_week<- tweet_time %>% group_by(week) %>% summarise(t_count = n(),.groups = "keep") %>%  ggplot(aes(x = reorder(week,t_count),y = t_count))+
  geom_bar(aes(fill= t_count),stat = "identity",position = "dodge")+
  coord_flip()+theme(axis.text.x = element_text(angle = 45))+
  labs(x = "days",y = "tweet count",title = "Tweet count of Executive branch by week",subtitle = "N of Account = 119\ntfrom: 01/12/19\ntill: 30/07/20")


tweet_time_month<- tweet_time %>% group_by(month) %>% summarise(t_count = n(),.groups = "keep") %>%  ggplot(aes(x = reorder(month,t_count),y = t_count))+
  geom_bar(aes(fill= t_count),stat = "identity",position = "dodge")+
  coord_flip()+theme(axis.text.x = element_text(angle = 45))+
  labs(x = "days",y = "tweet count",title = "Tweet count of Executive branch by week",subtitle = "N of Account = 119\ntfrom: 01/12/19\ntill: 30/07/20")

# Reply counts ------------------------------------------------------------

reply_dir <- "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/phd data/Phd_data_collection/Scripts/extended_list_collection/replies_to_actors"

reply_files<-list.files(path = reply_dir, pattern = "*.RDS",full.names = T)

reply_files<-reply_files[48:277]

eu_status_id <- unique(twt_npl_full_nrt$status_id)
eu_replies<-tibble()

repreader <- function(x){
  raw_reply<- readRDS(x)
  reply_to_eu<- filter(raw_reply,reply_to_status_id%in%eu_status_id)
  return(reply_to_eu)
}

eu_replies<- lapply(reply_files, FUN = repreader)

eu_replies<- do.call("rbind",eu_replies)

eu_user_replies <- filter(eu_replies,!user_id%in%non_pol$user_id)

eu_replies_summary<- eu_user_replies %>% group_by(reply_to_screen_name) %>% summarise(r_count = n()) %>%
  ggplot(aes(x= reorder(reply_to_screen_name,r_count),y= r_count))+
  geom_bar(aes(fill = r_count),stat = "identity",position = "dodge")+
  coord_flip()+labs(title = "Reply count of Executive branch",x="Executive Account",y="reply count",subtitle = "from:01/12/19\ntill:30/07/2020")

eu_replies_location<- eu_user_replies %>% group_by(country) %>% tally()
# Small test sample: ------------------------------------------------------

eng_test_sample <- twt_no_nrt %>%filter(lang == "en") %>%  select(screen_name,created_at, text) %>% sample_n(size = 15,replace = F)

write.csv(x = eng_test_sample,file = paste0(getwd(),"/test_sample_2.csv"),fileEncoding = "UTF-8")

# sampled accounts descriptive stats --------------------------------------

tweet_count_account <- trimmed_tweets %>% group_by(user_id) %>% tally(sort = T) %>% top_n(n = 100) %>% 
  ggplot(data = .,aes(x = reorder(user_id,n),y = n))+geom_bar(aes(fill = n), position = "dodge", stat = "identity")+coord_flip()+
  labs(x = "",y = "n",title = "number of tweets by handle", subtitle = "N of tweets > 500/Top 100 accounts")

ggsave(filename = paste0(graphDIR,"tweet_count_account.jpeg"),plot = tweet_count_account )

tweet_count_date <- trimmed_tweets %>% group_by(date = floor_date(date, "week")) %>% tally(sort = T) %>%
  ggplot(data = .,aes(x = as.character(date), y = n))+geom_bar(aes(fill = n), position = "dodge", stat = "identity")+coord_flip()+
  labs(x = "",y = "n",title = "number of tweets by date", subtitle = "time window: week")

ggsave(filename = paste0(graphDIR,"tweet_count_date.jpeg"),plot = tweet_count_date)


tweet_count_lang <- trimmed_tweets %>% group_by(lang) %>% tally(sort = T) %>% mutate(perc = ((n/sum(n))*100)) %>% filter(perc >=1) %>% 
  ggplot(data = .,aes(x = reorder(lang,perc),y = perc))+geom_bar(aes(fill = perc), position = "dodge", stat = "identity")+coord_flip()+
  labs(x = "",y = "percentage",title = "number of tweets by lang", subtitle = "% share >= 1 %")

ggsave(filename = paste0(graphDIR,"tweet_p_date.jpeg"),plot = tweet_count_date)



