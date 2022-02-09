# ICR learning script

install.packages("irr")

library(tidyverse)
library(irr)

data("diagnoses")

coder.data<- diagnoses[,1:3]

head(coder.data)

#calculating unweighted cohen's kappa
#2 rater; rater 1 and 2
uck<- irr::kappa2(ratings = coder.data[,c("rater1","rater2")],weight = "unweighted",sort.levels = )

uck$method
uck$value #gives the cohen's kappa
uck$raters #gives the number of raters
uck$subjects #gives the number of coding units

uck# gives an overview

#K's alpha test
#for k alpha, matrix rows represent raters and columns represent the units of coding

nmm<-matrix(c(1,1,NA,1,2,2,3,2,3,3,3,3,3,3,3,3,2,2,2,2,1,2,3,4,4,4,4,4,
              1,1,2,1,2,2,2,2,NA,5,5,5,NA,NA,1,1,NA,NA,3,NA),nrow=4)

#this matrix represents, 4 raters coding 12 items
#this will take some data wrangling...
nkalpha<- irr::kripp.alpha(x = nmm,method = "nominal")

nkalpha#gives an overview
