#Antiquities project
#Fitting a model

#files needed: 
# (i) "clean_file_with_extras.xlsx"
# (ii) "marginaleffects.xlsx"

#Vani Kanoria
#install.packages("readxl")
#install.packages("writexl")
#install.packages("lme4")    # for mixed effects models
#install.packages("sjPlot")  #for plotting lmer and glmer mods
#install.packages("effects")
#install.packages("predictmeans")
#install.packages("lmerTest")
#install.packages("ggeffects)
library("tidyverse")
library("readxl")
library("writexl")
library("patchwork")
library("lme4")
library(sjPlot) #for plotting lmer and glmer mods
library("effects")
library(predictmeans)
library("ggeffects")
library(lmerTest)
library(ggeffects)

#import data file (in tidy form) from excel
clean_file_with_extras<-read_excel("clean_file_with_extras.xlsx")

#Preprocessing data:
clean_file_with_extras$year<-as.numeric(clean_file_with_extras$year)
#centering year,
clean_file_with_extras$centred_year<-clean_file_with_extras$year-1985 # Center: year - first year
clean_file_with_extras<-clean_file_with_extras%>%rename(Ohio=`Ohio?`)
clean_file_with_extras<-clean_file_with_extras%>%rename(size=`size (in inches)`)
#dropping missing variables
data1<-clean_file_with_extras%>%drop_na(`appraised value`)%>%drop_na(size)
#fix the data error:
data1[496,'appraised value']<-32500
#Changing type of variables to factor variables
data1$lav = log(data1$`appraised value`)
data1$cid = data1$`Colgate cat. no.` 
data1$culture<-factor(data1$culture)
data1$medium<-factor(data1$medium)
data1$Ohio<-factor(data1$Ohio)
data1$centred_year<-as.numeric(data1$centred_year)
#final dataset: data1

#Final mixed effects model
#Fit a model that predicts price using type, size, culture with a random intercept on item id
mem8<-lmer(lav~culture+medium+size+poly(centred_year,2)+Ohio+(1|cid),data1)
summary(mem8)
#store residuals and fitted values in data1
data1$residuals_mem8<-residuals(mem8)
data1$fitted_mem8<-fitted(mem8)
#plot residuals
g1<-ggplot()+
  geom_point(data=data1,aes(x=fitted_mem8,y=residuals_mem8,color=factor(`Colgate cat. no.`)))+
  xlab("fitted values")+ylab("residuals")

#Interpretation: centred year
# (i) emtrends
library(emmeans)
trends_overall<-emtrends(mem8,~1,var="centred_year",
                         at=list(centred_year=c(20)))
test(emtrends(mem8,~1,var="centred_year"))
summary(mem8)
#calculating number of years to go over in for loop
year_range = data1%>% summarize(range=max(centred_year) - min(centred_year))
for (i in 0:as.numeric(year_range)){
  print(emtrends(mem8,~1,var="centred_year",
                 at=list(centred_year=c(i))))
}
#manually copied marginal effects by year (1985 to 2020) to an excel workbook (marginaleffects.xlsx)
marginaleffects<-read_excel("/Users/vani/Desktop/Art analysis/marginaleffects.xlsx")
#Plotting marginal effects:
ggplot(data = marginaleffects,aes(x=Years, y=centred_year.trend))+
  geom_point()+
  theme_bw()                    + #removes grey background 
  geom_hline(yintercept=0)+
  #scale_x_continuous(breaks=seq(1985,2020,by=1))+
  #scale_y_continuous(breaks=seq(0,30000,by=5000))+
  ggtitle("Marginal effects of years")+geom_path()+
  xlab("Year")  + #x axis label
  ylab("Marginal effects") +
  theme(axis.text.x = element_text(angle = 90))

#calculating %change by year but taking exp(marginaleffect)
marginaleffects$percent_change<-exp(marginaleffects$centred_year.trend)-1
#Plotting %change:

ggplot(data = marginaleffects,aes(x=Years, y=percent_change*100))+
  geom_point()+
  theme_bw()                    + #removes grey background 
  #geom_hline(yintercept=0)+
  scale_x_continuous(breaks=seq(1985,2021,by=2))+
  scale_y_continuous()+
  ggtitle("Marginal effect of year on log(appraised value)",
          subtitle = "according to results of mixed effects model")+geom_path()+
  xlab("Year")  + #x axis label
  ylab("average % change in appraised value") +
  theme(axis.text.x = element_text(angle = 90))

#(ii) interpretation: ggeffects: predictions
meff.plot<-ggeffect(model=mem8, c("centred_year [1:35]"))

plot(meff.plot)
data.frame(meff.plot)

meff.plot2<-ggeffect(model=mem8, c("centred_year [1:35]", "Ohio"))
plot(meff.plot2)
data.frame(meff.plot2)

meff.plot3<-ggeffect(model=mem8, c("centred_year [1:35]", "culture"))
plot(meff.plot3)
data.frame(meff.plot3)

meff.plot4<-ggeffect(model=mem8, c("centred_year [1:35]", "culture","Ohio"))
plot(meff.plot4)
data.frame(meff.plot4)

meff.plot5<-ggeffect(model=mem8, c("centred_year [1:35]", "size"))
plot(meff.plot5)
data.frame(meff.plot5)

meff.plot6<-ggeffect(model=mem8, c("centred_year [1:35]", "size", "Ohio"))
plot(meff.plot6)
data.frame(meff.plot6)

meff.plot7<-ggeffect(model=mem8, c("centred_year [1:35]", "medium"))
plot(meff.plot7)
data.frame(meff.plot7)
