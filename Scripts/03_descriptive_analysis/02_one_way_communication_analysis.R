###################################################################
# Title: One way communication indicators                         #
# Author: Sina Özdemir                                            #
#         PhD candidate                                           #
#         Department of sociology and political science           #
#         NTNU, Norway                                            #
# Date: 08/02/2022                                                #
###################################################################


# Setup -------------------------------------------------------------------


library(pacman)

packs<- c("tidyverse","here","patchwork","sjlabelled","ggrepel","viridis","patchwork","hrbrthemes","circlize","networkD3")

p_load(char = packs)

graphs_path<- here("Graphs")

data_path<- here("Data","Analysis_data")

data<- readRDS(file = file.path(data_path,"analysis_data_11042022.rds")) %>% 
  mutate(across(V301_01_Identity_and_mandate:V401_12_None,~as.integer(as.character(.x)))) %>% 
  mutate(Actor_type = replace_na(Actor_type, "Agency"),
         is_reply = replace_na(is_reply, 0))


# executive subject communication -----------------------------------------

##TODO: can't find a way to show who other actors are with a publication quality graph....

sop_data<- data %>%
  select(screen_name,Actor_type,status_id,V201_subject_of_publicity,V202_01_Subjects)


sop_data<- sop_data %>% 
  mutate(V202_01_Subjects = ifelse(grepl(x=V202_01_Subjects,pattern = "us|our|we")&(nchar(V202_01_Subjects)<=3),"Self",V202_01_Subjects))

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
  ggplot(aes(x = V201_subject_of_publicity, y= sop_perc))+
  geom_bar(aes(fill = sop_perc),stat="identity",position = "dodge")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,vjust=.7))+
  facet_wrap(~Actor_type)

# Overall Executive communication -----------------------------------------

overall_oop_freqs<- data %>%
  select(screen_name,Actor_type,status_id,matches(match = "V301_*"),is_reply) %>% 
  summarise(iam_perc = (sum(V301_01_Identity_and_mandate)/n()),
            output_perc = (sum(V301_02_Output)/n()),
            activity_perc = (sum(V301_03_Activity)/n()),
            opinion_perc = (sum(V301_04_Opinion)/n()),
            input_perc = (sum(V301_05_Input_seeking)/n()),
            other_perc = (sum(V301_06_Other)/n()),
            reply_perc = (sum(is_reply)/n())) %>% 
  mutate(across(iam_perc:other_perc, ~round(.x,2))) %>% 
  pivot_longer(cols = iam_perc:reply_perc,names_to = "object_type",values_to = "percentage") %>% 
  mutate(com_strat = ifelse(object_type %in% c("iam_perc","output_perc","activity_perc","opinion_perc","other_perc"),"one_way",
                            ifelse(object_type == "input_perc","two_way_asymetric","two_way_symetric"))) 
  
overall_oop_plot<- overall_oop_freqs %>%
  ggplot(aes(x = reorder(object_type,-percentage),y = percentage,fill = com_strat))+
  geom_bar(position = "dodge",stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1))+
  scale_fill_brewer(palette = "Dark2",
                    labels = c("One-way","Two-way asymmetric","Two-way symmetric"),
                    name = "Communication strategies")+
  scale_x_discrete(labels = c("output","activity","opinion","other","reply","mandate","input seeking"))+
  labs(x = "Object of communication", y= "Percentage")


ggsave(filename = "overall_oop_plot.jpeg",
       plot = overall_oop_plot,
       path = graphs_path,
       bg = "white",
       width = 5,
       height = 6,
       units = "in")


actor_oop_freqs<- data %>%
  select(screen_name,Actor_type,status_id,matches(match = "V301_*"),is_reply) %>% 
  group_by(Actor_type) %>% 
  summarise(iam_perc = (sum(V301_01_Identity_and_mandate)/n()),
            output_perc = (sum(V301_02_Output)/n()),
            activity_perc = (sum(V301_03_Activity)/n()),
            opinion_perc = (sum(V301_04_Opinion)/n()),
            input_perc = (sum(V301_05_Input_seeking)/n()),
            other_perc = (sum(V301_06_Other)/n()),
            reply_perc = (sum(is_reply)/n())) %>% 
  pivot_longer(cols = iam_perc:reply_perc,names_to = "object_type",values_to = "percentage") %>% 
  mutate(com_strat = ifelse(object_type %in% c("iam_perc","output_perc","activity_perc","opinion_perc","other_perc"),"one_way",
                            ifelse(object_type == "input_perc","two_way_asymetric","two_way_symetric")))


actor_oop_point<- actor_oop_freqs %>%
  group_by(object_type) %>% slice_max(n = 3,order_by = percentage) %>% 
  mutate(object_type = str_remove_all(string = object_type,"_perc")) %>% 
  ggplot(aes(x=percentage,y=percentage))+
  geom_point(aes(color = Actor_type,alpha = percentage),size = 3,show.legend = F)+
  geom_text_repel(aes(label = Actor_type),size = 2,max.overlaps = 30)+
  scale_color_brewer(palette = "Dark2")+
  facet_grid(rows = vars(object_type))+
  theme_bw()


ggsave(filename = "actor_oop_point.jpeg",
       plot = actor_oop_point,
       path = graphs_path,
       bg = "white",
       width = 5,
       height = 6,
       units = "in")

rm(list = ls())
