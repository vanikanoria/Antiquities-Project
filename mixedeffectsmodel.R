#Antiquities project
#Fitting a model

#Vani Kanoria
#install.packages("readxl")
#install.packages("writexl")
#install.packages("lme4")    # for mixed effects models
#install.packages("sjPlot")  #for plotting lmer and glmer mods
#install.packages("effects")
#install.packages("predictmeans")
#install.packages("lmerTest")
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

#import from excel
clean_file_with_extras<-read_excel("clean_file_with_extras.xlsx")

#simple linear regression
lm1<-lm(`appraised value`~culture+medium+`size (in inches)`+year,clean_file_with_extras)
summary(lm1)
lm2<-lm(`appraised value`~culture+medium+`size (in inches)`+`Ohio?`+year,clean_file_with_extras)
summary(lm2)
lm3<-lm(`appraised value`~year+`Colgate cat. no.`,clean_file_with_extras)
summary(lm3)

# Fit a model that predicts price using type, size, culture
# with a random intercept on item id
#fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
# y~x+x2+... + (1|ID)
# Only random intercept
# Allow the intercept to vary based on each individual
# Year : predictor
# Center: year - first year
clean_file_with_extras$year<-as.numeric(clean_file_with_extras$year)
clean_file_with_extras$centred_year<-clean_file_with_extras$year-1985
clean_file_with_extras<-clean_file_with_extras%>%rename(Ohio=`Ohio?`)
clean_file_with_extras<-clean_file_with_extras%>%rename(size=`size (in inches)`)

#random intercepts model
mem_ri<-lmer(`appraised value`~1+(1|`Colgate cat. no.`),clean_file_with_extras)
summary(mem_ri)

#adding year as a predictor
mem1<-lmer(`appraised value`~year+(1|`Colgate cat. no.`),clean_file_with_extras)
summary(mem1)
plot(mem1)

#adding centered year and other predictors
mem2<-lmer(`appraised value`~culture+medium+size+centred_year+(1|`Colgate cat. no.`),clean_file_with_extras)

summary(mem2)
# each item (`Colgate cat. no.`) has its own random intercept.

#include Ohio
mem3<-lmer(`appraised value`~culture+medium+size+centred_year+Ohio+(1|`Colgate cat. no.`),clean_file_with_extras)
summary(mem3)
extractAIC(mem3) #edf	(the ‘equivalent degrees of freedom’ for the fitted model fit): 16.00
# AIC:12454.83
#BIC:  12271.31
coef(mem3)

dim(clean_file_with_extras)
#residual plot
plot(mem3)

#log of the appraisal value:
data1<-clean_file_with_extras%>%drop_na(`appraised value`)%>%drop_na(size)
mem7<-lmer(log(`appraised value`)~culture+medium+size+centred_year+Ohio+(1|`Colgate cat. no.`),data1)
summary(mem7)
# add residuals as a column with the other data
data1$residuals_mem7<-residuals(mem7)
data1$fitted<-fitted(mem7)
# color the points by ID
ggplot()+
geom_point(data=data1,aes(x=fitted,y=residuals_mem7,color=factor(`Colgate cat. no.`)))+
  xlab("fitted values")+ylab("residuals")

#plot(residuals(mem7),col=clean_file_with_extras$`Colgate cat. no.`)
#residplot(mem7,group=clean_file_with_extras$`Colgate cat. no.`)

# look for outlier
# by matching data with residual observations
temp2<-clean_file_with_extras%>%drop_na(`appraised value`)
dim(temp2)
temp2$residuals_mem1<-temp$residuals.mem1.
temp2<-temp2%>%mutate(id=row_number())
temp2$id[temp2$residuals.mem1.>200000]
temp2[536:538,] #it's the 537th observation
#496th observation in data1

#fix the data error:
data1[496,'appraised value']<-32500

#adding square of age as a predictor
data1$centred_year_sq<-(data1$centred_year)^2
data1$lav = log(data1$`appraised value`)
data1$cid = data1$`Colgate cat. no.` 
data1$culture<-factor(data1$culture)
data1$medium<-factor(data1$medium)
data1$Ohio<-factor(data1$Ohio)
data1$centred_year<-as.numeric(data1$centred_year)

mem8<-lmer(lav~culture+medium+size+poly(centred_year,2)+Ohio+(1|cid),data1)
summary(mem8)
#plot residuals
data1$residuals_mem8<-residuals(mem8)
data1$fitted_mem8<-fitted(mem8)
g1<-ggplot()+
  geom_point(data=data1,aes(x=fitted_mem8,y=residuals_mem8,color=factor(`Colgate cat. no.`)))+
  xlab("fitted values")+ylab("residuals")
#look at outlier(residual<-0.2)
View(data1[abs(data1$residuals_mem8)>0.2,]) 

#interpreting the variable centred_year
library(emmeans)
trends_overall<-emtrends(mem8,~1,var="centred_year",
         at=list(centred_year=c(20)))
test(emtrends(mem8,~1,var="centred_year"))
summary(mem8)

#use a for loop
#put numbers in spreadsheet --> table with marginal effect of year --> exponentiate gives % change
#use emtrends to find the effect of year
#ask Prof. Marlowe if she has questions about particular years
#look at coefficient estimates of summary(mem8)
#then interaction terms: does time etc affect things differently

#calculating number of years to go over in for loop
year_range = data1%>% summarize(range=max(centred_year) - min(centred_year))

for (i in 0:as.numeric(year_range)){
  print(emtrends(mem8,~1,var="centred_year",
           at=list(centred_year=c(i))))
}
  
#manually copied marginal effects by year (1985 to 2020) to an excel workbook (marginaleffects.xlsx)
marginaleffects<-read_excel("/Users/vani/Desktop/Art analysis/marginaleffects.xlsx")

#Plotting marginal effects:
# Points and path
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
# Points and path
ggplot(data = marginaleffects,aes(x=Years, y=percent_change))+
  geom_point()+
  theme_bw()                    + #removes grey background 
  #geom_hline(yintercept=0)+
  #scale_x_continuous(breaks=seq(1985,2020,by=1))+
  scale_y_continuous()+
  ggtitle("Percentage change in average valuations as a result of year")+geom_path()+
  xlab("Year")  + #x axis label
  ylab("% change") +
  theme(axis.text.x = element_text(angle = 90))

#This is better:
library(ggeffects)
meff.plot<-ggeffect(model=mem8, c("centred_year [1:35]"))
plot(meff.plot)
dfmeff<-data.frame(meff.plot)
dfmeff$'predicted appraisal value'<-exp(dfmeff$predicted)
dfmeff$year<-dfmeff$x+1985
ggplot(data=dfmeff,aes(x=year,y=`predicted appraisal value`))+geom_line()+
  theme_bw()                    + #removes grey background 
  #geom_hline(yintercept=0)+
  scale_x_continuous(breaks=seq(1985,2020,by=2))+
  scale_y_continuous()+
  ggtitle("Percentage change in average valuations as a result of year")+
  xlab("Year")  + #x axis label
  ylab("% change") +
  theme(axis.text.x = element_text(angle = 90))

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

#Cleaner versions of the above plots
library(ggeffects)
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

#It should be 0:35? or 1:36?

#try interaction terms
#year:size
mem9<-lmer(log(`appraised value`)~culture+medium+size+poly(centred_year,degree=2)+Ohio+centred_year:size+(1|`Colgate cat. no.`),data1)
summary(mem9)
data1$residuals_mem9<-residuals(mem9)
data1$fitted_mem9<-fitted(mem9)
g2<-ggplot()+
  geom_point(data=data1,aes(x=fitted_mem9,y=residuals_mem9,color=factor(`Colgate cat. no.`)))+
  xlab("fitted values")+ylab("residuals")

#does the interaction term affect the residuals?
g1/g2

#effects
effects_culture <- effects::effect(term= "culture", mod= mem3)
summary(effects_culture) #output of what the values are
effects_medium<-effects::effect(term= "medium", mod= mem3)
summary(effects_medium) #output of what the values are
effects_Ohio<-effects::effect(term= "Ohio", mod= mem3)
summary(effects_Ohio)
effects_year<-effects::effect(term= "centred_year", mod= mem3)
summary(effects_year)

# Fit the model with some two-way interactions to see if anything is "going on"
mem4<-lmer(log(`appraised value`)~culture+medium+size+centred_year+centred_year_sq+Ohio+ centred_year:Ohio+(1|`Colgate cat. no.`),REML=T,data1)
#fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
summary(mem4)
#interactions are not significant

mem5<-lmer(log(`appraised value`)~culture+medium+size+centred_year+centred_year_sq+Ohio+ centred_year:culture+(1|`Colgate cat. no.`),data1)
summary(mem5)
#interactions are not significant

mem6<-lmer(log(`appraised value`)~culture+medium+size+centred_year+centred_year_sq+Ohio+ centred_year:size+(1|`Colgate cat. no.`),data1)
summary(mem6)
#Some predictor variables are on very different scales: consider rescaling 
#year:size is significant!!
AIC(mem3)
AIC(mem6) #<--- lower

#interpreting year using ggeffects:
ggpredict(mem8,'centred_year_sq')


#Using a cubic polynomial for year instead:
mem10<-lmer(lav~culture+medium+size+poly(centred_year,3)+Ohio+(1|cid),data1)
summary(mem10)
#plot residuals
data1$residuals_mem10<-residuals(mem10)
data1$fitted_mem10<-fitted(mem10)
g3<-ggplot()+
  geom_point(data=data1,aes(x=fitted_mem8,y=residuals_mem8,color=factor(`Colgate cat. no.`)))+
  xlab("fitted values")+ylab("residuals")

g1/g3
#g1 and g3 appear to be the same
#try ggeffects for mem10:
library(ggeffects)
meff.plot<-ggeffect(model=mem10, c("centred_year [1:35]"))
plot(meff.plot)
data.frame(meff.plot)

#investigating interaction term Ohio:centred_year
mem11<-lmer(lav~culture+medium+size+poly(centred_year,2)+Ohio+Ohio:centred_year+(1|cid),data1)
summary(mem11)
#plot residuals
data1$residuals_mem11<-residuals(mem11)
data1$fitted_mem11<-fitted(mem11)
g1<-ggplot()+
  geom_point(data=data1,aes(x=fitted_mem11,y=residuals_mem11,color=factor(`Colgate cat. no.`)))+
  xlab("fitted values")+ylab("residuals")
#not significant

#investigating interaction term size:centred_year
mem12<-lmer(lav~culture+medium+size+poly(centred_year,2)+Ohio+size:centred_year+(1|cid),data1)
summary(mem12)


