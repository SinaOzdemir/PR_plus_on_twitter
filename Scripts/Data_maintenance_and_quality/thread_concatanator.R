#Thread concatanator:

# notes -------------------------------------------------------------------

#1) there are tweets that don't follow the default thread structure
    #1) Some people just replied to the same tweet to form some sort of tweet cluster[DONE]

#2) Some threads start as a reply to a different account. I don't have the OG tweets[DONE]

#3) Some threads do follow the default thread structure but creation time differs
# there are tweets between thread members so the step-wise loop cannot concatenate them.


# to do -------------------------------------------------------------------

#Note number 3 should be done

#All data should be put back together(Note 1 + Note 2 + Note 3 + raw data)

# DIRs, packs and data --------------------------------------------------------------------

library("tidyverse")

dataDIR <- paste0(getwd(),"/Data/")

# Data --------------------------------------------------------------------

dta<- readRDS(file = paste0(dataDIR,"full_npl_nrt_1219_0720.RDS"))

dta_threads <- filter(dta, reply_to_screen_name == screen_name)

og_ids<- filter(dta_threads, !reply_to_status_id%in%status_id) %>% pull(reply_to_status_id) %>% na.omit()

og_twts<- filter(dta, status_id%in%og_ids)
og_twts<- filter(og_twts,is.na(reply_to_status_id))
threads<- rbind(dta_threads,og_twts)

threads<- threads %>% group_by(screen_name) %>% arrange(status_id,created_at) %>% select(screen_name,status_id,created_at,text,reply_to_status_id,reply_to_screen_name)

by_user<-split(threads,threads$screen_name)

saveRDS(object = threads,file = paste0(dataDIR,"threads_full_data.RDS"))
saveRDS(object = by_user, file = paste0(dataDIR,"threads_by_users.RDS"))

test_by_user = by_user

#this works

for (j in 1:length(test_by_user)) {

test_thread<-test_by_user[[j]]  

for (i in nrow(test_thread):1) {
  if(test_thread$reply_to_status_id[i]%in%test_thread$status_id[i-1]){
    test_thread$text[i-1] <- paste(test_thread$text[i-1],test_thread$text[i],sep = ".")
    test_thread<-test_thread[-i,]
  }
}
test_by_user[[j]]<-test_thread  
}

aggregated_threads<-do.call("rbind",test_by_user)
saveRDS(aggregated_threads,file = paste0(dataDIR,"aggregated_threads.RDS"))




# exceptional thread handling ---------------------------------------------

data<- readRDS(paste0(dataDIR,"aggregated_threads.RDS"))

#1) tweet clusters as threads
##shortcut

dups<- data %>% pull(reply_to_status_id) %>% na.omit()
dups<-dups[duplicated(dups)]

cluster_tweets<- data %>% filter(reply_to_status_id%in%dups | status_id %in% dups)

for (i in 1:nrow(cluster_tweets)) {
  if(cluster_tweets$status_id[i]%in%cluster_tweets$reply_to_status_id & is.na(cluster_tweets$reply_to_status_id[i])){
    cluster_tweets$cluster_id[i]<-cluster_tweets$status_id[i]
  }else{
    cluster_tweets$cluster_id[i]<-cluster_tweets$reply_to_status_id[i]
  }
    
  
}

cluster_tweets_a<- cluster_tweets %>%
  group_by(cluster_id,screen_name) %>%
  summarise(text = paste(text, collapse = ". ")) %>%
  rename(status_id = cluster_id)

cluster_tweets_final<- data %>%
  filter(status_id %in%cluster_tweets_a$status_id) %>%
  select(-text) %>%
  right_join(.,cluster_tweets_a,by = c("status_id","screen_name")) %>%
  saveRDS(.,file = paste0(dataDIR,"/cluster_tweets.RDS"))


#2) sporadic threads[couldn't solve this...]

