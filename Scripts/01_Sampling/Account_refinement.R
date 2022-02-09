#Account selection#


# Setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","here")

pacman::p_load(char = packs, update = F)
data_path <- here("Data","Main data")

account_info <- read.csv(file = file.path(data_path,"account_inf_vars_v2.csv"),
                         na.strings = " ",
                         stringsAsFactors = F,
                         encoding = "UTF-8")

analysis_data<- readRDS(file = file.path(data_path,"eu_data_011219_310720.RDS"))


# account extraction ----------------------------------------------------------------


analysis_accounts<- analysis_data %>% pull(screen_name) %>% unique()

close_exam<- account_info %>%
  filter(screen_name %in% analysis_accounts) %>%
  filter(Actor_type == "Institution")

write.table(x = close_exam,
            file = file.path(data_path,"analysis_accounts.txt"),
            quote = T,
            sep = ",",
            row.names = F,
            col.names = T,
            fileEncoding = "UTF-8")


# account filtering -------------------------------------------------------

imp_accounts<- account_info %>%
  filter(screen_name%in%analysis_accounts) %>%
  filter(Actor_type != "Institution")

inst_accounts<- read.csv(file = file.path(data_path,"take_out_accounts.csv"),
                         sep = ",",
                         encoding = "UTF-8",
                         stringsAsFactors = F) %>%
  rename(throw_out = X.U.FEFF.throw_out) %>% 
  filter(throw_out == 0) %>% select(-throw_out)

imp_accounts<- imp_accounts %>%
  select(-X.U.FEFF.) %>% 
  rbind(.,inst_accounts)


write.table(x = imp_accounts,
            file = file.path(data_path,"analysis_accounts_inf_vars.csv"),
            sep = ",",
            row.names = F,
            col.names = T,
            fileEncoding = "UTF-8")


a<-read.csv(file = file.path(data_path,"analysis_accounts_inf_vars.csv"))
