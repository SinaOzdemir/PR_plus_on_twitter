###################################################################
# Title: Subject of publicity  indicators                         #
# Author: Sina Özdemir                                            #
#         PhD candidate                                           #
#         Department of sociology and political science           #
#         NTNU, Norway                                            #
# Date: 08/02/2022                                                #
###################################################################


# Setup -------------------------------------------------------------------


library(pacman)

packs<- c("tidyverse","here","patchwork","igraph","ggraph","quanteda","ggrepel")

p_load(char = packs)

graphs_path<- here("Graphs")

data_path<- here("Data","Analysis_data")

data<- readRDS(file = file.path(data_path,"analysis_data_11042022.rds")) %>% 
  mutate(across(V301_01_Identity_and_mandate:V401_12_None,~as.integer(as.character(.x)))) %>% 
  mutate(Actor_type = replace_na(Actor_type, "Agency"),
         is_reply = replace_na(is_reply, 0)) %>% 
  mutate(V201_subject_of_publicity = ifelse(grepl(x=V202_01_Subjects,pattern = "my|us|our|we",ignore.case = T)&(nchar(V202_01_Subjects)<=3),
                                            "Self",
                                            V201_subject_of_publicity))






# SOP frequency analysis -----------------------------------------

##TODO: can't find a way to show who other actors are with a publication quality graph....

sop_data<- data %>%
  select(screen_name,
         Actor_type,
         status_id,
         V201_subject_of_publicity,
         V202_01_Subjects)


sop_summary<- sop_data %>% 
  group_by(V201_subject_of_publicity) %>% 
  summarise(sop_count = n()/nrow(sop_data))

actor_type<- sop_data %>% 
  group_by(Actor_type) %>% 
  summarise(a_tweet = n())

actor_type_sop_summary<- sop_data %>%
  group_by(Actor_type,V201_subject_of_publicity) %>% 
  summarise(sop_count = n()) %>% 
  left_join(.,actor_type,by = "Actor_type") %>% 
  mutate(sop_perc = round(sop_count/a_tweet,2)) %>% 
  mutate(label_y = cumsum(sop_perc) - 0.5 * sop_perc)

sop_graph<- actor_type_sop_summary %>% 
  ggplot(aes(x = reorder(as.factor(V201_subject_of_publicity),-sop_perc), y= sop_perc))+
  geom_bar(aes(fill = sop_perc),stat="identity",position = "dodge")+
  scale_fill_continuous(name = "Percentage")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,vjust=.7))+
  labs(x = "Subjet of publicity",y = "percentage")+
  facet_wrap(~Actor_type)



# SOP-OOP analysis --------------------------------------------------------

actor_type_message_n <- data %>%
  group_by(Actor_type,V201_subject_of_publicity) %>%
  summarise(tweet_n = n())

sop_oop<- data %>%
  group_by(Actor_type,V201_subject_of_publicity) %>% 
  summarise(across(V301_01_Identity_and_mandate:V301_06_Other,~sum(.x))) %>% 
  left_join(.,actor_type_message_n,by = c("Actor_type","V201_subject_of_publicity")) %>% 
  mutate(across(V301_01_Identity_and_mandate:V301_06_Other,~round((.x/tweet_n),2))) %>% 
  pivot_longer(cols = V301_01_Identity_and_mandate:V301_06_Other,names_to = "variables",values_to = "values")


sop_oop %>% 
  ggplot(aes(x=V201_subject_of_publicity,y= values))+
  geom_bar(aes(fill = variables),stat = "identity",position = "stack")+
  theme_bw()+
  facet_wrap(~Actor_type)





# network analysis --------------------------------------------------------

text_data<- sop_data %>%
  filter(V201_subject_of_publicity %in% c("Other actors","Compound")) %>% 
  select(screen_name,V202_01_Subjects) %>% 
  group_by(screen_name) %>% 
  summarise(mentions = paste(V202_01_Subjects,collapse = ":")) %>%
  mutate(mentions = trimws(mentions)) %>% 
  mutate(mentions = str_replace_all(string = mentions,pattern = " ",replacement = "_")) %>% 
  mutate(mentions = tolower(mentions)) %>% 
  mutate(mentions = str_replace_all(string = mentions,pattern = ":",replacement = " "))

mention_corp<- text_data %>% corpus(docid_field = "screen_name",text_field ="mentions" )

mention_tokens <- mention_corp %>%
  tokens(what = "word",
         remove_punct = T,
         remove_symbols = F,
         remove_numbers = F,
         include_docvars = T) %>% 
  tokens_select(pattern = stopwords(language = "en"),selection = "remove")



mentions_dfm<- mention_tokens %>%
  dfm() %>%
  convert(to = "data.frame") %>% 
  pivot_longer(cols = !doc_id ,
               names_to = "mentions",
               values_to = "frequency" )


account_message_n <- data %>%
  group_by(screen_name) %>%
  summarise(tweet_n = n()) %>% 
  rename(doc_id = screen_name)


mentions_graph_data<- mentions_dfm %>%left_join(.,account_message_n,by = "doc_id") %>% 
  mutate(mention_perc = round((frequency/tweet_n),2)) %>% 
  group_by(doc_id) %>%
  slice_max(order_by = frequency,n = 1)
  
mentions_lollipop <- mentions_graph_data %>%
  filter(frequency > 3 & doc_id != "EUombudsman") %>% 
  ggplot(aes(x= doc_id,y = mention_perc))+
  geom_segment(aes(x = doc_id,xend = doc_id, y= 0,yend = mention_perc),color = "steelblue")+
  geom_point(aes(size = (mention_perc),alpha = (mention_perc)),color = "#366442",show.legend = F)+
  geom_text_repel(aes(label = mentions),nudge_y = 0.01,min.segment.length = 0,size = 5)+
  theme_bw()+
  coord_flip()+
  labs(x = "EU executives",y = "% share",caption = "Minimum frequency is set at 4")


##TODO: text preprocessing steps
##  1) Aggregate to account
##  2) fix encoding??
##  3) Remove punctuations and mentions (, and @)
##  4) conver to lower case
##  5) remove stopwords
##  6) count the frequency
##  7) join with original data
##  8) network graph/lollipop graph the most frequent ones

