library("tidyverse")
#install.packages("readxl")
library("readxl")

#import from excel
antiq_file<-read_excel("Analysis1.xlsx")

#bring into tidy format: pivot by date

antiq1<-antiq_file%>%pivot_longer(
  cols = c(starts_with("19"),starts_with("20")), 
  names_to = "year", 
  values_to = "appraised value")
antiq2<-antiq1[,c(6,1:14)]
antiq3<-antiq2[,c(-3,2:5,8:12,14:15)]



