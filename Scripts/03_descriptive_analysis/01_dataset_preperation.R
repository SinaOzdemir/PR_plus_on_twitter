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

packs<- c("tidyverse","here","xlsx")

p_load(char = packs)

graphs_path<- here("Graphs")

data_path<- here("Data","Analysis_data")



# get data ----------------------------------------------------------------

###invalid input found on input connection is due to a special character
### need to find a solution for this...

data_a<-xlsx::read.xlsx(file = file.path(data_path,"data_PR_plus_2022-03-08_13-28-KG.xlsx"),
                        sheetIndex = 1,
                        header = T,encoding = "UTF-16") %>%
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
  
  

data<-xlsx::read.xlsx(file = file.path(data_path,"data_PR_plus_2022-04-11_11-58.xlsx"),
                        sheetIndex = 1,
                        header = T,encoding = "UTF-16") %>%
  select(-SERIAL,-REF) %>% 
  filter(FINISHED ==1) %>% 
  filter(nchar(V101_02)>0) %>% 
  filter(CI02 == "Live") %>% 
  mutate(V101_01 = as.character(V101_01)) %>% 
  mutate(v101_02 = as.character(V101_02)) %>% 
  mutate(STARTED = strptime(x = STARTED, format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(screen_name = str_remove_all(string = V101_02,pattern = "https://twitter.com/|/status/|[[:digit:]]+")) %>% 
  mutate(status_id = str_extract_all(string = V101_02, pattern = "/[:digit:]+"),
         status_id = str_remove_all(string = status_id, pattern = "/")) %>% 
  filter(!(status_id%in%data_a$status_id)) %>% 
  rbind(data_a,.)


# remove the duplicated observations used in intercoder reliability test

icr_datasets<- list.files(data_path,pattern = "ICR_Test_data",full.names = T) %>% 
  map_dfr(.x = .,.f = read.xlsx,sheetIndex = 1) %>% 
  select(status_id, status_link) %>% 
  mutate(status_link = str_extract_all(string = status_link, pattern = "/[:digit:]+"),
         status_link = str_remove_all(string = status_link, pattern = "/")) %>% 
  mutate(status_extraction = as.character(ifelse(is.na(status_link),status_id,status_link))) %>% 
  pull(status_extraction)

data<- data %>% filter(!(CI01 %in% c("KG","PdW") & status_id%in%icr_datasets))




#retain recoded tweets
dup_id<- data %>% filter(duplicated(status_id)) %>% pull(status_id)

data_w_dupes<- data %>%
  filter(status_id%in%dup_id) %>%
  group_by(status_id) %>% 
  filter(STARTED == max(STARTED))

data<- data %>%
  filter(!(status_id%in%dup_id)) %>% 
  rbind(.,data_w_dupes)

#assign proper names to variables
variable_keys<- read.csv(file = file.path(data_path,"variables_PR_plus_2022-02-08_16-16.csv"),
                         sep = "\t",
                         header = T,
                         fileEncoding = "UTF-16")

variable_keys<- variable_keys %>%
  mutate(LABEL = str_remove_all(string = LABEL,pattern = "[[:digit:]]|\\[|\\]"),
         LABEL = str_remove_all(string = LABEL,pattern = "object_of_publicity: |policy_area: "),
         LABEL = str_remove_all(string = LABEL, pattern = ",|\\:"),
         LABEL = trimws(x = LABEL,which = "right"),
         LABEL = str_replace_all(string = LABEL, pattern = " ",replacement = "_")) %>% 
  filter(grepl(pattern = "^CI|^V|^T",x = VAR,perl = T)) %>% 
  rowwise() %>% 
  mutate(LABEL = paste(VAR,LABEL,sep = "_"))

data<- data %>% select(screen_name,status_id,any_of(variable_keys$VAR))

colnames(data)<- c("screen_name","status_id",variable_keys$LABEL)

# combine api information with coded information

api_data<- readRDS(file = file.path(data_path,"eu_data_011219_310720.RDS")) %>% 
  filter(status_id %in% data$status_id) %>%
  select(screen_name,status_id,followers_count,reply_to_status_id,
         reply_to_screen_name,retweet_screen_name,
         quoted_screen_name,is_quote,is_retweet,
         favorite_count,retweet_count,quote_count,reply_count) %>% 
  mutate(is_reply = ifelse(is.na(reply_to_screen_name),0,1))

#add manually coded information on accounts to the dataset

account_info <- read.csv(file = file.path(data_path,"analysis_accounts_inf_vars.csv"),
                         header = T,
                         sep = ",",
                         stringsAsFactors = F) %>% 
  select(Actor_name,Actor_type,screen_name)

analysis_data<- left_join(data,api_data,by = c("screen_name","status_id")) %>% 
  left_join(.,account_info, by = "screen_name") %>% filter(!duplicated(status_id)) %>% 
  mutate(across(V301_01_Identity_and_mandate:V401_12_None,~recode_factor(.x, Yes = 1L, No = 0L))) %>%
  mutate(across(V301_01_Identity_and_mandate:V401_12_None,~as.integer(as.character(.x)))) %>% 

saveRDS(object = analysis_data, file = file.path(data_path,"analysis_data_11042022.rds"))

rm(list = ls())
