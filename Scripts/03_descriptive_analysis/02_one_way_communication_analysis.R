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

graphs_path<- here("Graphs","Prelim_analysis_080222")

data_path<- here("Data","Prelim_results_data")

data<- readRDS(file = file.path(data_path,"analysis_data_080222.rds"))

# Frequencies -------------------------------------------------------------

account_freq<- data %>% group_by(screen_name) %>% tally()

oop_freq<- data %>%
  select(screen_name,V301_01_Identity_and_mandate:V301_05_Input_seeking) %>% 
  pivot_longer(cols = V301_01_Identity_and_mandate:V301_05_Input_seeking, names_to = "object_of_publicity",values_to = "value") %>% 
  group_by(screen_name,object_of_publicity,value) %>%
  tally() %>%
  filter(value == "Yes") %>% rename(oop_freq = n) %>% 
  drop_na() %>% left_join(.,account_freq,by = "screen_name") %>% 
  mutate(oop_perc = (oop_freq/n))


overall_oop_perc_graph<- oop_freq %>%
  group_by(object_of_publicity) %>%
  summarise(n = sum(oop_freq),
            n_perc = (sum(oop_freq)/nrow(data))) %>% 
  ggplot(aes(x = reorder(object_of_publicity,n_perc), y= n_perc))+
  geom_bar(aes(fill = n_perc),stat = "identity", position = "dodge")+
  theme_bw()+
  coord_flip()+
  labs(x = "Message type", y = "Percentage",title = "Overall share of message types",subtitle = paste0("N = ", nrow(data)))

ggsave(overall_oop_perc_graph,filename = "overall_oop_perc.jpeg",path = graphs_path,bg = "white")  
# 
# oop_perc_by_account_graph<- oop_freq %>%
#   ggplot(aes(x = reorder(screen_name,oop_freq), y = oop_freq))+
#   geom_bar(aes(fill = object_of_publicity),stat = "identity",position = "stack")+
#   coord_flip()+
#   theme_minimal()+
#   labs(x = "Twitter handle",
#        y = "Frequency",
#        title = "Overview of message types by the EU executives")+
#   guides(fill = guide_legend(title="Message Type"))
# 

top_accounts<- oop_freq %>%
  group_by(object_of_publicity) %>%
  top_n(n = 2,wt = oop_freq) %>% ggplot(aes(x= n, y = oop_freq))+
  geom_point(aes(size = oop_freq, color = screen_name),show.legend = F)+
  geom_text_repel(aes(label = screen_name))+
  facet_grid(rows =vars(object_of_publicity))+
  theme_bw()+
  guides(size = guide_legend("Message type frequency"), color = "none")+
  labs(title = "Peak accounts by message type", x= "Total number of message",y= "Frequency of message types")

ggsave(top_accounts,filename = "top_accounts_by_message_type.jpeg",path = graphs_path,bg = "white")

output_freq<- data %>%
  filter(V301_02_Output == "Yes") %>%
  group_by(Actor_type) %>%
  tally() %>%
  rename(oop_output_freq = n)

output_transparency_data<- data %>%
  filter(V301_02_Output == "Yes") %>%
  group_by(Actor_type,V201_subject_of_publicity) %>%
  tally() %>% rename(sop_freq =n) %>% left_join(.,output_freq,by = "Actor_type") %>% 
  drop_na() %>% mutate(sop_output_perc = (sop_freq/oop_output_freq))

transparency_overall <- output_transparency_data %>%
  group_by(V201_subject_of_publicity) %>% summarise(n = sum(sop_output_perc)) %>% 
  ggplot(aes(x = reorder(V201_subject_of_publicity,-n), y = n))+
  geom_bar(aes(fill = n), position = "dodge",stat = "identity")+
  theme_bw()+
  labs(x = "Mentioned actor type",y = "Percentage", title = "Percentage share of mentioned actors in output messages")

ggsave(transparency_overall,filename = "transparency_overall_perc.jpeg", path = graphs_path,bg = "white")
# 
# output_transparency_tile_dta<- output_transparency_data %>%
#   pivot_wider(id_cols = screen_name,names_from = V201_subject_of_publicity,values_from = sop_output_perc,values_fill = 0) %>% 
#   pivot_longer(cols = Compound:None,names_to = "actor_type",values_to = "perc")
# 
# output_transparency_graph<- output_transparency_tile_dta %>%
#   ggplot(aes(x = reorder(screen_name,perc),y = reorder(actor_type,perc)))+
#   geom_tile(aes(fill = perc),color = "grey50")+coord_flip()+
#   scale_fill_distiller(direction = 1)+
#   theme_bw()+
#   labs(x = "Actor type", y = "Screen Name", title = "Percentage share of actor\ntypes in EU executive messages")
# 
# 
# ggsave(output_transparency_graph,filename = "output_transperncy_by_actor.jpeg",path = graphs_path,bg = "white")

peak_transparents<- output_transparency_data %>% 
  filter(V201_subject_of_publicity %in% c("Self","Compound")) %>% 
  group_by(Actor_type) %>% 
  summarise(transparent_messages = sum(sop_freq),
            transparency_perc = ((sum(sop_freq))/(sum(oop_output_freq)))) %>% 
  ggplot(aes(x= transparent_messages, y = transparency_perc))+
  geom_point(aes(color = Actor_type),show.legend = F)+
  geom_text_repel(aes(label = Actor_type),max.overlaps = 30)+
  theme_bw()+
  guides(size = guide_legend("Message type frequency"), color = "none")+
  labs(title = "Transparency", x= "Total number of message",y= "Percentage share of responsibility reporting")

ggsave(peak_transparents,filename = "peak_transparent_atypes.jpeg",path = graphs_path,bg = "white")
