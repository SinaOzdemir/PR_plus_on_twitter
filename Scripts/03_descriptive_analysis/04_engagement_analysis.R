###################################################################
# Title: Engagement analysis                                      #
# Author: Sina Özdemir                                            #
#         PhD candidate                                           #
#         Department of sociology and political science           #
#         NTNU, Norway                                            #
# Date: 09/02/2022                                                #
###################################################################


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","here","margins","patchwork")

p_load(char = packs)

graphs_path<- here("Graphs","Prelim_analysis_080222")

data_path<- here("Data","Prelim_results_data")

data<- readRDS(file = file.path(data_path, "analysis_data_080222.rds"))


# create DV ---------------------------------------------------------------

data<- data %>%
  mutate(engagement_count = ((retweet_count+favorite_count)/followers_count)) %>% 
  mutate(engagement_log = log(engagement_count,base = exp(1))) %>% 
  filter(engagement_log != -Inf)

count_density<-data %>%
  ggplot(aes(x = engagement_count))+
  geom_density()+
  theme_bw()

log_density<- data %>% 
  ggplot(aes(x = engagement_log))+
  geom_density()+
  theme_bw()

density_plots<-count_density+log_density

ggsave(filename = "dv_density_plots.jpeg",plot = density_plots,path = graphs_path,bg = "white")


# additional IVS ----------------------------------------------------------

data<- data %>% 
  mutate(reply_to_others = ifelse(screen_name!=reply_to_screen_name,1,0)) %>% 
  mutate(reply_to_others = replace_na(reply_to_others, 0))

# Linear regression -------------------------------------------------------
regression_data<- data %>% 
  select(status_id,engagement_log,matches(match = "^V3|V201_subject_of_publicity"),reply_to_others) %>% 
  mutate(across(V301_01_Identity_and_mandate:V301_06_Other, ~recode(.x, Yes = 1, No = 0))) %>% 
  mutate(V201_subject_of_publicity = as.factor(V201_subject_of_publicity)) %>% 
  mutate(V201_subject_of_publicity = relevel(V201_subject_of_publicity, ref = "None"))

IVs<- c(grep(pattern = "^V3|V201_subject_of_publicity",x = colnames(regression_data),value = T),"reply_to_others")

rhs<- paste(IVs,collapse = "+")

DV<- "engagement_log"

model_form<- as.formula(object = paste(DV,"~",rhs))

model_fit<- lm(formula = model_form,data = regression_data)

summary(model_fit)

marg_effects<- margins_summary(model_fit)

marg_effects<- marg_effects %>% 
  mutate(AME_Perc = ((exp(AME)-1)*100)) %>% 
  mutate(p_value = ifelse( p <= 0.05,1,0)) %>% 
  mutate(p_value = as.factor(p_value))


reg_results<- marg_effects %>% 
  ggplot(aes(x= reorder(factor,-p), y = AME))+
  geom_errorbar(aes(ymin = lower,ymax = upper),width = .1)+
  geom_point(aes(color = p_value),size = 4,show.legend = F)+
  geom_hline(yintercept = 0,color = "red")+
  theme_bw()+
  coord_flip()+
  labs(title = "Linear regression results",
       subtitle = paste0("N =", nrow(regression_data)),
       caption = "Blue features are significant at .05\nReference category for subject of publicity is *None*",
       x = "",
       y = "Average marginal effect")
 
mean_differences<- marg_effects %>% 
  ggplot(aes(x = reorder(factor,-p),y = AME_Perc))+
  geom_bar(aes(fill = p_value),stat = "identity",position = "dodge",show.legend = F)+
  theme_bw()+
  coord_flip()+
  labs(title = "Average difference in geometric means",
       subtitle = paste0("N = ", nrow(regression_data)),
       caption = "Difference bars in blue are significant at .05\nReference category for subject of publicity is None",
       x = "",
       y = "Average difference in means")


full_reg_results<- reg_results+mean_differences

