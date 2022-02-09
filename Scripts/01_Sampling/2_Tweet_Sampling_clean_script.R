#Sampling tweets: NEEDS TO BE REVISED, DO NOT RUN
#Code chunks that start with (D) is deprecated and should not be run
#they are kept for archival purpuses
# packages: ---------------------------------------------------------------
packs <- c("tidyverse","lubridate","stringr","stringi","here")

lapply(packs, library, character.only = T)

# DIRs: -------------------------------------------------------------------

dataDIR <- here("Data","Main data")
graphDIR<- here("Graphs")

# Data: -------------------------------------------------------------------

pop <- readRDS(file = file.path(dataDIR,"full_data_011219_310720.RDS"))

accounts<- read.csv(file = file.path(dataDIR,"analysis_accounts_inf_vars.csv"),
                      sep = ",",
                      stringsAsFactors = F,
                      encoding = "UTF-8") %>%
  pull(screen_name)

trimmed_pop<- pop %>% filter(screen_name %in%accounts)

# Categorical variables for sampling ------------------------------------------------------

# i. N of Hashtags
# ii. Mentions
# iii. Tweet length
# iv. retweet count
# v. fav count
#emoji regex:(\u00a9|\u00ae|[\u2000-\u3300]|\ud83c[\ud000-\udfff]|\ud83d[\ud000-\udfff]|\ud83e[\ud000-\udfff])

for (i in 1:nrow(trimmed_pop)) {
  print(paste0(i," out of ",nrow(trimmed_pop)))
  trimmed_pop$hashtag_count[i] = length(trimmed_pop$hashtags[[i]])
  trimmed_pop$mention_count[i] = length(trimmed_pop$mentions_user_id[[i]])
}


trimmed_pop$text_length <- trimmed_pop$text %>%
  #remove emojies
  iconv(from = "UTF-8",to = "ASCII",sub = "" ) %>%
  #remove mentions
  str_remove_all(string = .,pattern = "^@[\\w]+|\\s@[\\w]+") %>%
  #remove twtitter links
  str_remove_all(string = ., pattern = "http[s]://t.co/[^ ]+") %>% 
  #remove whitespaces
  trimws() %>%
  #count characters
  nchar(trimmed_pop$text,type = "chars")

#remove 0 character length 
trimmed_pop<-filter(trimmed_pop, text_length != 0)

usr_ids <- unique(trimmed_pop$user_id)
trimmed_pop_final = tibble()
#create categories for sampling cells  
for (i in 1:length(usr_ids)) {
  print(paste0(i," out of ",length(usr_ids)))
  
  actor_data <- filter(trimmed_pop, user_id %in% usr_ids[i])
  
  leng_quant <- quantile(actor_data$text_length, type = 8)
  
  actor_data$text_length_cat <- ifelse(actor_data$text_length <= leng_quant[["25%"]],"short",
                                     ifelse(actor_data$text_length >leng_quant[["25%"]] & actor_data$text_length < leng_quant[["75%"]],"medium",
                                            ifelse(actor_data$text_length >=leng_quant[["75%"]],"long","indeterminate")))
  
  fav_quant <- quantile(actor_data$favorite_count,type = 8)
  
  actor_data$fav_cat <- ifelse(actor_data$favorite_count <= fav_quant[["25%"]],"few",
                                       ifelse(actor_data$favorite_count >fav_quant[["25%"]] & actor_data$favorite_count < fav_quant[["75%"]],"medium",
                                              ifelse(actor_data$favorite_count >=fav_quant[["75%"]],"many","indeterminate")))
  
  
  share_quant <- quantile(actor_data$retweet_count, type = 8)
  
  actor_data$share_cat <- ifelse(actor_data$retweet_count <= share_quant[["25%"]],"few",
                               ifelse(actor_data$retweet_count >share_quant[["25%"]] & actor_data$retweet_count < share_quant[["75%"]],"medium",
                                      ifelse(actor_data$retweet_count >=share_quant[["75%"]],"many","indeterminate")))
  
  trimmed_pop_final<- rbind(trimmed_pop_final,actor_data)
  
}

# Stratified (by account and text length) random sample: ----------------------------------

#Determining the strata sizes for the variabels representative on accounts,

#tweet share of the account & tweet length

samp_size<- 4603

#calculate sample proportions:

sampling_data <- trimmed_pop_final %>%
  group_by(user_id,text_length_cat,fav_cat,share_cat) %>%
  summarise(abs_freq = n()) 

actor_prop <- trimmed_pop_final %>%
  group_by(user_id) %>%
  summarise(tweet_n = n()) %>%
  mutate(strt_prop = tweet_n/sum(tweet_n),
         act_samp_size = round(strt_prop*samp_size))

samp_props <- left_join(sampling_data,actor_prop, by = "user_id") %>%
  mutate(strata_rel_freq = abs_freq/tweet_n) %>%
  mutate(strt_size_act = round(act_samp_size*strata_rel_freq))

#don't coerce the extremely rare groups
# samp_props$strt_size_act<- ifelse(samp_props$strt_size_act == 0 ,
#                                   samp_props$abs_freq,
#                                   samp_props$strt_size_act) 

samp_props <- select(samp_props, user_id:share_cat,strt_size_act)


saveRDS(samp_props,file = paste0(dataDIR,"str_rand_samp_size.RDS"))

#Draw the sample
set.seed(8292)

final_sample <- trimmed_pop_final %>%
  group_by(user_id,text_length_cat,fav_cat,share_cat) %>%
  nest() %>%
  ungroup() %>%
  left_join(x = .,y=samp_props,by = c("user_id","text_length_cat","fav_cat","share_cat")) %>% 
  mutate(samp = map2(.x = data, .y = strt_size_act,.f = sample_n)) %>%
  select(-data) %>% unnest(cols = samp) 

saveRDS(final_sample, file = file.path(dataDIR,"full_coding_sample_01112021.RDS"))

save_Data<- final_sample %>%
  select(status_id,created_at,text,screen_name,lang) %>%
  mutate(status_id = paste0("x",.$status_id))


# Representativeness check (by time) --------------------------------------

final_sample$weeks<- lubridate::floor_date(x = final_sample$created_at,unit = "week")

twt_sample_time_dist<- final_sample %>% group_by(weeks) %>% tally()
trimmed_pop_final$weeks<- lubridate::floor_date(x = trimmed_pop_final$created_at,unit = "week")
pop_time_dist <- trimmed_pop_final %>% group_by(weeks) %>% summarise(pop_time_dist = n())


twt_time_dist<- left_join(twt_sample_time_dist,pop_time_dist,by = "weeks")

twt_time_dist<- twt_time_dist %>% mutate(sample_perc = (n/nrow(final_sample)),
                                         pop_perc = (pop_time_dist/nrow(trimmed_pop_final)),
                                         diff = (sample_perc-pop_perc))

rep_plot<-ggplot(data = twt_time_dist,aes(x = weeks, y = diff)) +
  geom_bar(aes(fill = diff),stat = "identity",position = "dodge")+coord_flip()+
  labs(title = "Percentage difference between sample and population",subtitle = "by weeks",x = "Time",y = "% difference")

ggsave(filename = "representativeness.png",plot = rep_plot,path = graphDIR,dpi = "retina")



# Recode emojis and line breaks and other nasty stuff ---------------------
devtools::install_github("https://github.com/hadley/emo",force = T)
library(emo)

emoji_decoder_2 <- data.frame(emoji_text = names(ji_name),
                              emoji_code = as.character(ji_name))

emoji_decoder_2$emoji_code<-as.character(emoji_decoder_2$emoji_code)

emoji_decoder_2$emoji_text <- paste0(" [",emoji_decoder_2$emoji_text,"] ")

emoji_decoder_a<-grep(pattern = "*asteri|keycap_star",x = emoji_decoder_2$emoji_text,ignore.case = T,value = T)

emoji_decoder_2 <- emoji_decoder_2[-which(emoji_decoder_2$emoji_text%in%emoji_decoder_a),]

for (i in 1:nrow(emoji_decoder_2)) {
  
  print(paste0("replacing ",emoji_decoder_2$emoji_text[i],"regex is ", emoji_decoder_2$emoji_code[i]))
  
  save_Data$text<- str_replace_all(string = save_Data$text,
                                    pattern = emoji_decoder_2$emoji_code[i],
                                    replacement = emoji_decoder_2$emoji_text[i])
}

#remove things that make reading rather hard:
save_Data$text<-gsub("&","and",save_Data$text)

save_Data$text<-gsub("andamp","and",save_Data$text)

save_Data$text<-gsub(";",",",save_Data$text)

save_Data$text<-gsub("\r?\n|\r"," ",save_Data$text)

#clean the remaining unix code for the emojis

emoji_regex<-"\\p{So}|\\p{Cn}|\U0001f3fb|\U0001f3fc"

save_Data$text_clean<- str_remove_all(string = save_Data$text,pattern = emoji_regex)

#save the data
save_Data %>%
   saveRDS(object = .,file = file.path(dataDIR,"coding_sample_01112021.RDS"))

save_Data %>%
  write.table(x = .,file = file.path(dataDIR,"coding_sample_01112021.txt"),
              sep = ";",col.names = T,row.names = F,fileEncoding = "UTF-8",qmethod = "double")

save_Data %>%
  write.table(x = .,file = file.path(dataDIR,"coding_sample_01112021.csv"),
              sep = ";",col.names = T,row.names = F,fileEncoding = "UTF-8",qmethod = "double")

SoSci_data<- save_Data %>%
  select(screen_name,status_id) %>% 
  mutate(status_id = str_remove_all(string = status_id,pattern = "x")) %>% 
  mutate(status_link =  paste("https://twitter.com",.$screen_name,"status",.$status_id,sep = "/")) %>% 
  select(-screen_name)

write.table(x = SoSci_data,file = file.path(dataDIR,"coding_sample_sosci_db_01112021.csv"),
            sep = ",",
            col.names = T,
            row.names = F,
            fileEncoding = "UTF-8",
            qmethod = "double")