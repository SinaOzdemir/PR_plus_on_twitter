
# Interpretation test -----------------------------------------------------

packs<- c("tidyverse","xlsx")

lapply(packs, library, character.only = T)

coding_cm<- read.xlsx(file = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 2 self-legitimation on Social Media/Content_analysis/Tests/test_sample_CM.xlsx",sheetIndex = 1,as.data.frame = T,encoding = "UTF-8") %>% na.omit()

coding_sfo<- read.xlsx(file = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/Papers/Paper 2 self-legitimation on Social Media/Content_analysis/Tests/test_sample_3_sfo.xlsx",sheetIndex = 1,as.data.frame = T,encoding = "UTF-8") %>% na.omit()

cnames.a<-colnames(coding_cm)[4:24]


perc.agreement = vector(mode = "integer",length = length(cnames.a))

for (i in 1:length(cnames.a)) {
  var.a <- coding_cm[,cnames.a[i]]
  var.b<- coding_sfo[,cnames.a[i]]
  var.match<- ifelse(var.a%in%var.b,1,0)
  perc.agree <- (sum(var.match)/length(var.match))
  names(perc.agreement)[i]<- cnames.a[i]
  perc.agreement[i]<- perc.agree
}

diff<- perc.agreement[which(perc.agreement<1)]


#safety and well being:
test_data <- coding_cm[,1:3]
sw<-cbind(test_data,coding_cm$V_240_safety_welbeing,coding_sfo$V_240_safety_welbeing)

sw$match<- ifelse(sw$V1%in%sw$V2,1,0)

#equality:

eq<-cbind(test_data,coding_cm$V_270_equal,coding_sfo$V_270_equal)

eq$diff<- ifelse(eq$`coding_cm$V_270_equal`==eq$`coding_sfo$V_270_equal`,0,1)

#fair

fa<- cbind(test_data, coding_cm$V_270_fair,coding_sfo$V_270_fair)

fa$diff<- ifelse(fa$`coding_cm$V_270_fair`== fa$`coding_sfo$V_270_fair`,0,1)

#free

#rule of law:

rl<- cbind(test_data, coding_cm$V_270_rule_law, coding_sfo$V_270_rule_law)

rl$diff<-ifelse(rl$`coding_cm$V_270_rule_law` == rl$`coding_sfo$V_270_rule_law`,0,1)
