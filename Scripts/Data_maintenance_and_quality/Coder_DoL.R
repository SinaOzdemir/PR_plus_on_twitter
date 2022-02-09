## Coding sample division##
library(tidyverse)
library(here)
data<- read.csv("C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 2 PR+ on Twitter/Analysis/Coding_links.csv",
                header = T,
                stringsAsFactors = F,
                encoding = "UTF-8")


coders<- c("SFO","KG","PdW")

data_portions<-c(3/6,2/6,1/6)

sampling_data<- data
for (i in 1:length(coders)) {
  
  coding_data<- sampling_data
  
  coding_sample<- slice_sample(coding_data,prop = data_portions[i],replace = F)
  
  sampling_data<- filter(sampling_data, !(X.U.FEFF.status_id%in%coding_sample$X.U.FEFF.status_id))
  
  write.table(x = coding_sample,
              file = paste0("C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 2 PR+ on Twitter/Analysis","/coding_links_",coders[i],".csv"),
              quote = T,
              sep = ",",
              row.names = F,
              col.names = T,
              fileEncoding = "UTF-8")
}