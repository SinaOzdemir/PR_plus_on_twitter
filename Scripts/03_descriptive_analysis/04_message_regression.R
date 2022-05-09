###################################################################
# Title: Regression analysis                                      #
# Author: Sina Özdemir                                            #
#         PhD candidate                                           #
#         Department of sociology and political science           #
#         NTNU, Norway                                            #
# Date: 15/04/2022                                                #
###################################################################


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("multinom","tidyverse","patchwork","here","lme4")

p_load(char = packs)


data_path<- here("Data","Analysis_data")

graph_path<- here("Graphs","draft_2_graphs")


data<- readRDS(file = file.path(data_path,"analysis_data_cluster.rds"))



# regression test ---------------------------------------------------------


reg_data<- data %>%
  select(status_id,cluster_n,Actor_type) %>% 
  mutate(Actor_type = replace_na(Actor_type, "Agency")) %>% 
  mutate(Actor_type = str_replace_all(string = Actor_type,"-| ",replacement = "_")) %>% 
  mutate(cluster_n = relevel(x = factor(cluster_n),ref = 4))

## Results of multinominal regression is a little hard to interpret
##  due to multiple actor types and reference group
##  for example the graph at the end tells us, High representatives are 80% more likely
##  to send a tweet from cluster 1 than a tweet in cluster 4
##  Therefore I decided to use binary logistic regression to estimate the likelihood
## multinominal logistic regression----
# library(nnet)
# 
# mnl_fit<- multinom(cluster_n ~ Actor_type, data = reg_data )
# 
# mnl_fit_z<-summary(mnl_fit)$coefficients/summary(mnl_fit)$standard.errors
# 
# mnl_fit_p <- (1 - pnorm(abs(mnl_fit_z), 0, 1)) * 2
# 
# mnl_fit_coefs<-exp(coef(mnl_fit))
# 
# mnl_pp<- fitted(mnl_fit) %>% as.data.frame()
# 
# colnames(mnl_pp)<-c("cluster_4","cluster_1","cluster_2","cluster_3")
# 
# mnl_pred_prob_df<- reg_data %>%
#   select(Actor_type) %>%
#   cbind(.,mnl_pp) %>% group_by(Actor_type) %>% 
#   summarise(cl_4_pp = mean(cluster_4),
#           cl_1_pp = mean(cluster_1),
#           cl_2_pp = mean(cluster_2),
#           cl_3_pp = mean(cluster_3)) %>% 
#   pivot_longer(cols = cl_4_pp:cl_3_pp,names_to = "variables",values_to = "predicted_probability")
# 
# mnl_pred_prob_graph<- mnl_pred_prob_df %>% 
#   ggplot(aes(x = Actor_type,y = predicted_probability))+
#   geom_bar(aes(fill=predicted_probability),stat = "identity",position = "dodge")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 45,vjust = .9))+
#   labs(subtitle = "reference group: cluster 4")+
#   facet_grid(rows = vars(variables))



# binary logistic regression ----------------------------------------------

cluster_groups<- unique(reg_data$cluster_n)

bin_log_res<- list()
for (i in 1:length(cluster_groups)) {
  model_data<- reg_data %>% 
    mutate(cluster_n = ifelse(cluster_n == cluster_groups[i],1,0))
  
  bin_model<- glm(cluster_n ~ Actor_type,
                  family = binomial(link = "logit"),
                  data = model_data)
  
  bin_log_res[[i]]<-bin_model
  
  names(bin_log_res)[i]<- paste0("cluster_",cluster_groups[i])
}

conf_int_calculator<- function(model){
  
  coefs<- summary(model)$coefficients %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "variables")
  
  coefs$upper<- exp((coefs$Estimate+(1.96*coefs$`Std. Error`)))
  
  coefs$lower<- exp((coefs$Estimate-(1.96*coefs$`Std. Error`)))
  coefs$Estimate<- exp(coefs$Estimate)
  return(coefs)
}


models_graph_dat<- data.frame()
for (i in 1:length(bin_log_res)) {
  
  model_res<- conf_int_calculator(bin_log_res[[i]])
  
  model_res$cluster_name<- rep(names(bin_log_res[i]),times = nrow(model_res))
  
  models_graph_dat<- rbind(models_graph_dat,model_res)
  
  
}


#dunno why this looks messed up
# models_res_graph<- models_graph_dat %>%
#   filter(variables != "(Intercept)") %>% 
#   mutate(variables = str_remove_all(string = variables,pattern = "Actor_type")) %>%
#   mutate(upper = ifelse(is.infinite(upper),0,upper)) %>% 
#   ggplot(aes(x = variables, y = round(Estimate,2)))+
#   geom_point(color = "skyblue")+
#   geom_errorbar(aes(ymin=lower, ymax=upper),color = "#800020",width = .01) +
#   theme_bw()+
#   coord_flip()+
#   facet_grid(cols = vars(cluster_name))


bin_log_res_df<- models_res_graph<- models_graph_dat %>%
  filter(variables != "(Intercept)") %>% 
  mutate(variables = str_remove_all(string = variables,pattern = "Actor_type")) %>%
  mutate(upper = ifelse(is.infinite(upper),0,upper))

saveRDS(bin_log_res_df,file = here("Results","logit_results.rds"))


a<- readRDS(here("Results","logit_results.rds"))

a$point_color<- ifelse(a$Estimate>1,1,0)

a$significance<- ifelse(a$`Pr(>|z|)`>=0 & a$`Pr(>|z|)` <=.001,"***",
                        ifelse(a$`Pr(>|z|)`>=.001 & a$`Pr(>|z|)` <= .05,"**",
                               ifelse(a$`Pr(>|z|)`>=.05 & a$`Pr(>|z|)` <=.1,"*"," ")))

a$variables<- gsub("_"," ",a$variables)

a$cluster_name<- recode(a$cluster_name,
                        cluster_1 = "All-in-one",
                        cluster_2 = "Activity-output",
                        cluster_3 = "Non-political",
                        cluster_4 = "Pure-output")

a %>% ggplot(aes(x = variables, y = Estimate))+
  geom_point(aes(color = as.factor(point_color), size = Estimate))+
  geom_text(aes(label = significance),nudge_y = 2)+
  guides(size = "none",color = guide_legend(title="Odds"))+
  scale_color_manual(labels = c("Negative", "Positive"), values = c("red", "green"))+
  coord_flip()+
  theme_bw()+
  facet_grid(cols = vars(cluster_name))+
  labs(x = "Account type",y = "Log-odds",caption = "Significance levels: *** = .001, ** = .05, * = .1")