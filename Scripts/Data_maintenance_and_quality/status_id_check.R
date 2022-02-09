library(pacman)

packs<- c("tidyverse","here","patchwork","sjlabelled","ggrepel","viridis","patchwork","hrbrthemes","circlize","networkD3")

p_load(char = packs)



# data --------------------------------------------------------------------


data<- read.csv(file = "data_PR_plus_2022-01-15_14-16.csv",header = T,sep = ",",na.strings = c(" ","","NA"),fileEncoding = "UTF-16") %>% 
  mutate(V101_01 = as.character(V101_01))

data<- data %>% select(CASE,V101_01,V101_02) %>% mutate(status_id = gsub("[^0-9]","",V101_02))
data<- data %>% drop_na()

coding_sample <- readRDS(file.choose())

coding_sample<- coding_sample %>% mutate(status_id = str_remove_all(string = status_id,pattern = "x"))


matching_coded_data<- data %>% filter(V101_01%in%coding_sample$status_id)

unmatching_coded_data<- data %>% filter(!(V101_01%in%coding_sample$status_id))

library(rtweet)


token<- rtweet::create_token(app = "Functionised_collector",
                             consumer_key = "WhBXWSk406xPBy2YC8f3KQ2gE",
                             consumer_secret = "szsHDExv4jiGiqEQ6qVwifTcKHZxPOZrv1BfYAHfhj0zyScy6R",
                             access_token = "1151438784384421888-p8wKQI4mkVvC6tTdDixe8sJG7MgS79",
                             access_secret ="fXULzyXbH2xz2r9TOLTSx4Zcvk6bMzsO59kAmeAVEoeUv",set_renv = F)


missing_tweets<- lookup_tweets(statuses = unmatching_coded_data$V101_01,token = token)

full_coding_sample<- readRDS(file.choose())

missing_a<- data %>% filter(!(V101_01%in%full_coding_sample$retweet_status_id))


full_data<- readRDS(file.choose())

missing_data_B<- data %>% filter(!(V101_01%in%full_data$status_id))


sosci_db<- read.csv(file = file.choose(),header = T,sep = "\t",na.strings = c(""," ","NA"),fileEncoding = "UTF-16")

sosci_linked<- sosci_db %>% filter(!is.na(D1)) %>% mutate(status_id = gsub("[^0-9]", "", D0)) %>% pull(status_id)

sosci_unlinked<- sosci_db %>% filter(is.na(D1))%>% mutate(status_id = gsub("[^0-9]", "", D0)) %>% pull(status_id)

sosci_db_match<- sosci_linked[(sosci_linked%in%sosci_unlinked)]
#so all sosci linked ones match all sosci unlinked ones... which means unlinked ones are duplicated....

coded_unmatch_a<- data %>% filter(!(V101_01%in%sosci_linked))

coded_unmatch_b<- data %>% filter(!(V101_01%in%sosci_unlinked))

full_sosci_db<- sosci_db %>% mutate(status_id = gsub("[^0-9]","",D0)) %>% pull(status_id)

coded_unmatch_c<- data %>% filter(!(V101_01%in%full_sosci_db))

coded_unmatch_d<- data %>% filter(!(status_id%in%full_sosci_db))

coded_unmatch_e<- data %>% filter(!(status_id%in%coding_sample$status_id))


coding_unmatch<- coding_sample %>% mutate(status_id_from_link = gsub("[^0-9]","",status_link)) %>% filter(!(status_id_from_link%in%full_sosci_db))


## status id variable is messed up,
#use sosci links to extract ids (D1) or twitter links (D0)
#to extract the status id's and match it with full dataset!!