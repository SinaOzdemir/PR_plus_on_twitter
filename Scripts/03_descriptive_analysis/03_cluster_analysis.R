###################################################################
# Title: Message clustering                                       #
# Author: Sina Özdemir                                            #
#         PhD candidate                                           #
#         Department of sociology and political science           #
#         NTNU, Norway                                            #
# Date: 13/04/2022                                                #
###################################################################


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("cluster","tidyverse","patchwork","here","NbClust","factoextra")

p_load(char = packs)


data_path<- here("Data","Analysis_data")

graph_path<- here("Graphs","draft_2_graphs")


data<- readRDS(file = file.path(data_path,"analysis_data_11042022.rds"))

object_data<- data %>%
  select(status_id,matches(match = "V301_"),is_reply) %>% 
  filter(!duplicated(status_id)) %>%
  mutate(is_reply = replace_na(is_reply,0))

# Clustering ------------------------------------------------

object_data<- object_data %>%
  mutate(across(V301_01_Identity_and_mandate:is_reply,~as.numeric(as.character(.x)))) %>% 
  column_to_rownames(var = "status_id")

# determine the relevant number of clusters using 22 indices:
jac_sim<- stats::dist(x = object_data,method = "binary")

set.seed(13042022)
test_stat<-c("kl",
             "ch",
             "hartigan",
             "ccc",
             "scott",
             "marriot",
             "trcovw",
             "tracew",
             "friedman",
             "rubin",
             "cindex",
             "db",
             "silhouette",
             "beale",
             "ratkowsky",
             "ball",
             "ptbiserial",
             "gap",
             "mcclain",
             "dunn",
             "sdindex",
             "sdbw")

##hubert and d index are graphical tests, should be run seperately
## gamma,gplus,tau takes too long to run

test_results<-data.frame()
for (i in 1:length(test_stat)) {
  
  cat("calculating ",test_stat[i],"\n")
  
  obj_clust_test_err<- NbClust(data = object_data,
                               diss = jac_sim,
                               distance = NULL,
                               min.nc = 1,
                               max.nc = 7,
                               method = "ward.D2",
                               index = test_stat[i])
  

  obj_clust_i_res<- data.frame(index_name = test_stat[i],
                               n_clust = obj_clust_test_err$Best.nc["Number_clusters"],
                               index_val = obj_clust_test_err$Best.nc["Value_Index"])
  test_results<- rbind(test_results, obj_clust_i_res)

}

saveRDS(object = test_results,file = here("Results","nbcluster_count_results.rds"))

obj_clust_hubert_test<- NbClust(data = object_data,
                             diss = jac_sim,
                             distance = NULL,
                             min.nc = 1,
                             max.nc = 7,
                             method = "ward.D2",
                             index = "hubert")
obj_clust_dindex_test<- NbClust(data = object_data,
                             diss = jac_sim,
                             distance = NULL,
                             min.nc = 1,
                             max.nc = 7,
                             method = "ward.D2",
                             index = "dindex")

table(test_results$n_clust)

cluster_index<- test_results %>%
  group_by(n_clust) %>%
  summarise(vote = n()) %>% 
  ggplot(aes(x= as.factor(n_clust), y = vote))+
  geom_bar(aes(fill = vote),stat = "identity",position = "dodge")+
  theme_bw()+
  labs(x = "number of clusters",y = "vote count")

ggsave(filename = "cluster_indices_results.jpeg",
       plot = cluster_index,
       path = graph_path,
       width = 4,
       height = 5,
       bg = "white")
  

#summarise test results

obj_hiercluster<- hclust(d = jac_sim,method = "ward.D2" )

dendo<-fviz_dend(x = obj_hiercluster,
          k = 4,
          k_colors = c("#F10060","steelblue","darkgreen","orange"),
          show_labels = F,
          type = "rectangle",
          main = "Hierarchical Clustering with Ward method",
          xlab = "tweets",
          rect_border = "black")

obj_tree_trim<- cutree(obj_hiercluster,k = 4)

obj_clusters<- data.frame(status_id = names(obj_tree_trim),
                          cluster_n = obj_tree_trim)

obj_clustered<- object_data %>%
  rownames_to_column(var = "status_id") %>% 
  left_join(x = .,y = obj_clusters, by = "status_id")


cluster_sum <- obj_clustered %>%
  group_by(cluster_n) %>% 
  summarise(total = n(),
            iam = sum(V301_01_Identity_and_mandate),
            output = sum(V301_02_Output),
            activity = sum(V301_03_Activity),
            opinion = sum(V301_04_Opinion),
            input = sum(V301_05_Input_seeking),
            other = sum(V301_06_Other),
            reply = sum(is_reply)) %>% 
  pivot_longer(cols = total:reply, names_to = "variables", values_to = "values")

cluster_n<- cluster_sum %>%
  filter(variables == "total") %>% 
  ggplot(aes(x = cluster_n, y= values))+
  geom_bar(aes(fill = values),stat = "identity",position = "dodge")+
  theme_bw()+
  labs(x = "Clusters", y = "N of tweets in clusters")

cluster_insight<- cluster_sum %>% filter(variables != "total") %>% 
  ggplot(aes(x = variables,y = values))+
  geom_bar(aes(fill = values),position = "dodge",stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = .7))+
  labs(x= "object of publicity",y = "count",title = "Cluster content distribution")+
  facet_grid(cols =  vars(cluster_n))

cluster_details<-(dendo+cluster_n)/cluster_insight

ggsave(filename = "cluster_details.jpeg",plot = cluster_insight,path = graph_path,width = 5,height = 6,units = "in",bg = "white")

saveRDS(object = obj_clusters,file = here("Results","cluster_results.rds"))

data<- data %>%
  left_join(.,y= obj_clusters, by = "status_id") 

saveRDS(data,file = here("Data","analysis_data_cluster.rds"))
account_n <- data %>% 
  group_by(Actor_type) %>% 
  summarise(message_count = n())

account_cluster<- data%>% 
  group_by(Actor_type,cluster_n) %>% 
  summarise(cluster_count = n()) %>% 
  mutate(Actor_type = replace_na(Actor_type, "Agency")) %>% 
  left_join(.,account_n, by = "Actor_type") %>% 
  mutate(cluster_perc = round((cluster_count/message_count),2)) %>% 
  ggplot(aes(x = reorder(as.factor(cluster_n),-cluster_perc),y = cluster_perc))+
  geom_bar(aes(fill = cluster_perc),stat = "identity",position="dodge")+
  theme_bw()+
  labs(x = "Cluster of tweets",y = "% share of clusters")+
  facet_wrap(~Actor_type)
  

