###################################################################
# Title: Two-way asymmetric and symmetric communication indicators#
# Author: Sina Özdemir                                            #
#         PhD candidate                                           #
#         Department of sociology and political science           #
#         NTNU, Norway                                            #
# Date: 27/01/2022                                                #
###################################################################


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","here","patchwork","ggrepel","igraph")

p_load(char = packs)

data_path<- here("Data","Prelim_results_data")

graph_path<- here("Graphs","Prelim_analysis_080222")

data<- readRDS(file = file.path(data_path,"analysis_data_080222.rds"))

# Indicator extraction----------------------------------------------------
account_stats <- data %>%
  group_by(screen_name,Actor_type) %>%
  summarise(n = n(),
            reply_count = sum(is_reply),
            retweet_count = sum(is_retweet),
            quote_count = sum(is_quote)) %>% 
  mutate(reply_rate = (reply_count/n),
         retweet_rate = (retweet_count/n),
         quote_rate = (quote_count/n)) %>% 
  drop_na() %>% 
  select(screen_name,Actor_type, n:quote_rate) %>% 
  ungroup()

##BBI produces NAs for rates which means it is not in the OG dataset

# Engagement analysis -----------------------------------------------------


## Engagement rate by actor type
rates_graph_data<- account_stats %>% group_by(Actor_type) %>% 
  summarise(reply_rate = (sum(reply_rate)/sum(n)),
            retweet_rate = (sum(retweet_rate/sum(n))),
            quote_rate = (sum(quote_rate)/sum(n))) %>% 
  pivot_longer(cols = reply_rate:quote_rate, names_to = "engagement_type",values_to = "engagement_rate")

rate_graph<- rates_graph_data %>%drop_na() %>% 
  ggplot(aes(x = reorder(as.factor(engagement_type),-engagement_rate),y = engagement_rate))+
  geom_bar(aes(fill = engagement_type),position = "dodge",stat = "identity")+
  theme_bw()+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  labs(x = "",
       y= "Percentage",
       title = "Different engagement types by actors as percentage share of total message volume")+
  scale_fill_manual(labels = c("quote_rate" = "Quote",
                               "reply_rate" = "Reply",
                               "retweet_rate" = "Retweet"),
                     values = c("quote_rate" = "#a1152f",
                                "reply_rate"= "#1b6dcc",
                                "retweet_rate" = "#4ea31d"))+
  facet_wrap(~Actor_type)

ggsave(filename = "engagement_by_actor_type.jpeg",plot = rate_graph,bg = "white",path = graph_path)

# Network analysis --------------------------------------------------------

## Retweet network

sn_dta<- data %>% 
  group_by(screen_name,retweet_screen_name) %>% 
  summarise(weight = n()) %>% 
  drop_na() %>% 
  rename(from = screen_name,
         to = retweet_screen_name) %>% 
  filter(weight >1)
  
sn_Actors<- data.frame(name = c(sn_dta$from,sn_dta$to)) %>%
  filter(!duplicated(name))

library(igraph)

sna<- graph_from_data_frame(sn_dta,directed = T,vertices = sn_Actors)

library(ggraph)


retweet_graph<-ggraph(sna)+
  geom_node_point()+
  geom_node_text(aes(label = names(V(sna))),
                 repel = T,
                 show.legend = F)+
  geom_edge_link(aes(edge_color = E(sna)$weight,
                     edge_alpha = E(sna)$weight),
                 arrow = arrow(type = "closed",length= unit(2,"mm")))+
  scale_edge_colour_continuous(
    low = "white",
    high = "black",
    space = "Lab",
    na.value = "grey50",
    guide = "edge_colourbar" )+
  scale_edge_alpha(guide = "none")+
  theme_graph()+
  labs(title = "Retweet network",
       subtitle = "Edge weight <2 are suppressed",
       caption = "Arrows indicate who is retweeted by whom\n eg: EU Near -> Commission)\ndarker links indicate higher frequency of retweet")


ggsave(filename = "retweet_network.jpeg",plot = retweet_graph,path = graph_path,bg = "white")




# Conversational replies: -----

#TODO: get the full dataset, extract reply to EU posts, and then extract replies to those replies...

