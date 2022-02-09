###################################################################
# Title: Dataset preperation                                      #
# Author: Sina Özdemir                                            #
#         PhD candidate                                           #
#         Department of sociology and political science           #
#         NTNU, Norway                                            #
# Date: 08/02/2022                                                #
###################################################################


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","here","patchwork","sjlabelled","ggrepel","viridis","patchwork","hrbrthemes","circlize","networkD3")

p_load(char = packs)

graphs_path<- here("Graphs")

data_path<- here("Data","Prelim_results_data")



# get data ----------------------------------------------------------------

###invalid input found on input connection is due to a special character
### need to find a solution for this...

data<- read.csv(file = file.path(data_path,"data_PR_plus_2022-02-08_16-08.csv"),
                header = T,
                sep = ",",
                quote = "",
                na.strings = c(""," ","NA")) %>%
  select(-SERIAL,-REF) %>% 
  filter(FINISHED ==1) %>% 
  filter(nchar(V101_02)>0) %>% 
  filter(CI02 == "Live") %>% 
  mutate(V101_01 = as.character(V101_01)) %>% 
  mutate(v101_02 = as.character(V101_02)) %>% 
  mutate(STARTED = strptime(x = STARTED, format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(screen_name = str_remove_all(string = V101_02,pattern = "https://twitter.com/|/status/|[[:digit:]]+")) %>% 
  mutate(status_id = str_extract_all(string = V101_02, pattern = "/[:digit:]+"),
         status_id = str_remove_all(string = status_id, pattern = "/"))

#retain recoded tweets
dup_id<- data %>% filter(duplicated(status_id)) %>% pull(status_id)

data_w_dupes<- data %>%
  filter(status_id%in%dup_id) %>%
  group_by(status_id) %>% 
  filter(STARTED == max(STARTED))

data<- data %>%
  filter(!(status_id%in%dup_id)) %>% 
  rbind(.,data_w_dupes)

variable_keys<- read.csv(file = file.path(data_path,"variables_PR_plus_2022-02-08_16-16.csv"),
                         sep = "\t",
                         header = T,
                         fileEncoding = "UTF-16")

variable_keys<- variable_keys %>%
  mutate(LABEL = str_remove_all(string = LABEL,pattern = "[[:digit:]]|\\[|\\]"),
         LABEL = str_remove(string = LABEL,pattern = "object_of_publicity: |policy_area: "),
         LABEL = str_remove(string = LABEL, pattern = ",|\\:"),
         LABEL = trimws(x = LABEL,which = "right"),
         LABEL = str_replace_all(string = LABEL, pattern = " ",replacement = "_")) %>% 
  filter(grepl(pattern = "^CI|^V|^T",x = VAR,perl = T)) %>% 
  rowwise() %>% 
  mutate(LABEL = paste(VAR,LABEL,sep = "_"))

data<- data %>% select(screen_name,status_id,any_of(variable_keys$VAR))

colnames(data)<- c("screen_name","status_id",variable_keys$LABEL)

api_data<- readRDS(file = file.path(data_path,"eu_data_011219_310720.RDS")) %>% 
  filter(status_id %in% data$status_id) %>%
  select(screen_name,status_id,reply_to_status_id,reply_to_screen_name,retweet_screen_name,quoted_screen_name,is_quote,is_retweet,favorite_count,retweet_count,quote_count,reply_count) %>% 
  mutate(is_reply = ifelse(is.na(reply_to_screen_name),0,1))

account_info <- read.csv(file = file.path(data_path,"analysis_accounts_inf_vars.csv"),
                         header = T,
                         sep = ",",
                         stringsAsFactors = F,
                         fileEncoding = "UTF-8") %>% 
  select(Actor_name,Actor_type,screen_name)

#remove duplicated rows for now. there should be a point where latest input should be chosen!

analysis_data<- left_join(data,api_data,by = c("screen_name","status_id"))

analysis_data<- left_join(analysis_data,account_info, by = "screen_name") %>% filter(!duplicated(status_id))

saveRDS(object = analysis_data, file = file.path(data_path,"analysis_data_080222.rds"))

rm(list = ls())
