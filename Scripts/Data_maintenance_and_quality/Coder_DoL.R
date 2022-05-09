## Coding sample division##
library(tidyverse)
library(here)
data<- read.csv("C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 2 PR+ on Twitter/Analysis/Coding_links.csv",
                header = T,
                stringsAsFactors = F,
                encoding = "UTF-8") %>%
  rename(status_id = X.U.FEFF.status_id) %>% 
  mutate(status_id = str_extract_all(string = status_link, pattern = "/[:digit:]+")) %>% 
  mutate(status_id = str_remove_all(string = status_id , pattern = "/")) %>% 
  mutate(status_id = as.character(status_id))


coders<- c("SFO","KG","PdW")

data_size<- c(nrow(slice_sample(.data = data,prop = 3/6)),
              nrow(slice_sample(.data = data,prop = 2/6)),
              nrow(slice_sample(.data = data,prop = 1/6)))

sampling_data<- data



for (i in 1:length(coders)) {
  
  coding_data<- sampling_data
  
  if( i != 3){
  coding_sample<- slice_sample(coding_data,n = data_size[i],replace = F)
  
  sampling_data<- filter(sampling_data, !(status_id%in%coding_sample$status_id))
  
  write.table(x = coding_sample,
              file = paste0("C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 2 PR+ on Twitter/Analysis","/revised_coding_links_",coders[i],".csv"),
              quote = T,
              sep = ",",
              row.names = F,
              col.names = T,
              fileEncoding = "UTF-8")
  }else{
    write.table(x = coding_data,
                file = paste0("C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 2 PR+ on Twitter/Analysis","/revised_coding_links_",coders[i],".csv"),
                quote = T,
                sep = ",",
                row.names = F,
                col.names = T,
                fileEncoding = "UTF-8")
  }
}