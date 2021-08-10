#Antiquities project
#Fitting a model

#files needed: 
# (i) "clean_file_with_extras.xlsx"
# (ii) "marginaleffects.xlsx"

#Vani Kanoria
############################################################################
### Load Libraries
############################################################################
library("tidyverse")    # For working with data
library("readxl")       # For reading from Excel file
library("writexl")      # For writing to Excel file
library("patchwork")    # For combining plots
library("lme4")         # For fitting mixed effects model
library("lmerTest")     # For fitting mixed effects model
library("sjPlot")       # For plotting lmer and glmer mods
library("effects")      # For marginal effects
library("predictmeans") # For estimated marginal means
library("emmeans")    # For marginal effects
library("ggeffects")    # For estimated marginal means

############################################################################
### Load Data
############################################################################
#import data file (in tidy form) from excel
clean_file_with_extras<-read_excel("clean_file_with_extras.xlsx")

####################################
### Preprocessing data
####################################
clean_file_with_extras$year<-as.numeric(clean_file_with_extras$year)
#centering year
clean_file_with_extras$centred_year<-clean_file_with_extras$year-1985 # Center: year - first year
#rename variables
clean_file_with_extras<-clean_file_with_extras%>%rename(Ohio=`Ohio?`)
clean_file_with_extras<-clean_file_with_extras%>%rename(size=`size (in inches)`)
#dropping missing variables
#data1<-clean_file_with_extras%>%drop_na(`appraised value`)%>%drop_na(size)
#NOTE: R should auto-drop when necessary
clean_file_with_extras <- clean_file_with_extras%>% mutate(postreform=case_when(year<=2002 ~ "Pre",
                                                                                year>2002 ~ "Post",
                                                                                TRUE ~ NA_character_))
data1 <- clean_file_with_extras

####################################
### Fix data entry errors
####################################
#fix the data error:
data1[which(data1$'appraised value' == 325000),'appraised value']<-32500

#changing medium from black fig to ceramic
data1[which(data1$medium=="black fig"),"medium"]<-"ceramic"

####################################
### Transforming / Factorizing data
####################################
data1$lav = log(data1$`appraised value`)
data1$cid = data1$`Colgate cat. no.` 
data1$culture<-factor(data1$culture)
data1$medium<-factor(data1$medium)
data1$Ohio<-factor(data1$Ohio)
data1$centred_year<-as.numeric(data1$centred_year)
data1$postreform<-factor(data1$postreform, levels = c("Pre","Post"))

gaps<-data1%>%filter(is.na(`appraised value`)==FALSE)
tiff("antiquities.tiff",width = 6, height = 4, units = "in", res = 300)
ggplot(data=data1, aes(x=year, y=`appraised value`, group=`Colgate cat. no.`,
                       color=factor(`Colgate cat. no.`)), show.legend=F)+
  geom_line(aes(linetype="Available Data"))+
  geom_point(size=0.75)+
  theme_bw()+
  xlab("Year")+
  ylab("Appraised Value")+
  ggtitle("Appraised Value of Antiquities",
          subtitle = "1985-2020") +
  geom_line(data = gaps,aes(linetype = "Missing Data")) + guides(color=FALSE)+
  scale_linetype_manual("", values =c(1,3))
dev.off()
############################################################################
### Fit Mixed Effects Model
############################################################################

####################################
### Fit model using:
###       type
###       size 
###       culture
###       year (polynomial to resolve residuals)
###       random intercept on id
####################################
#data1a<- data1 %>% filter(year<=2002)
data1b<- data1 %>% filter(year>2002)
mod<-lmer(lav~culture+medium+size+centred_year+centred_year+Ohio+(1|cid),data1)
mod.sum<-summary(mod)

library(MuMIn)
r.squaredGLMM(mod)

df<-mod.sum$coefficients
df<-cbind(rownames(df),df)
write_csv(path = "mod1-all.csv", x = data.frame(df))

############################################################################
### Check Model Assumptions
############################################################################
####################################
### Plot Residuals
####################################
dat.mod<-model.frame(mod)
dat.mod$residuals<-rstudent(mod) 
dat.mod$fitted<-fitted(mod)

#plot residuals
g1<-ggplot()+
  geom_point(data=dat.mod,aes(x=fitted,y=residuals))+
  #geom_point(data=dat.mod,aes(x=fitted,y=residuals,color=factor(cid)), show.legend = F)+
  xlab("Fitted")+
  ylab("Studentized Residual")+
  theme_bw()

## Outliers:
dat.mod %>% filter(abs(residuals)>3) 
# 90.3300 -- starting value (not unreasonable)
# 90.390  -- ending value   (not unreasonable)
# 91.300  -- starting value (not unreasonable)
# 90.106  -- ending value   (not unreasonable)
# 90.780  -- starting value (not unreasonable)
# 90.780  -- ending value   (not unreasonable)
# 90.104  -- starting value (not unreasonable)
# 90.116  -- ending value   (not unreasonable)

############################################################################
### Post hoc testing
############################################################################
pairs(emmeans(mod, ~medium))
pairs(emmeans(mod, ~culture))

####################################
### Check Trend of Return by year
####################################
year.range <- data1%>% summarize(range=max(centred_year) - min(centred_year))
yearly.return <-NULL
for (i in 0:as.numeric(year.range)){
  curr.row<-data.frame(emtrends(mod,~1,var="centred_year",
                                at=list(centred_year=c(i))))
  curr.row<-c("Year"=i+1985, curr.row)
  yearly.return <- rbind(yearly.return,unlist(curr.row))
}
yearly.return <- data.frame(yearly.return)
#calculating %change by year by taking exp(marginaleffect)
yearly.return$percent_change<-exp(yearly.return$centred_year.trend)-1

ggplot(data = yearly.return,aes(x=Year, y=percent_change*100))+
  geom_point()+
  geom_hline(yintercept=0)+
  theme_bw()                    + #removes grey background 
  ggtitle("Marginal Effect of Year on Appraised Value",
          subtitle = "Over 1985-2020 Based on the Model")+geom_path()+
  xlab("Year")  + #x axis label
  ylab("Percent Change in Appriased Value Over a Year")


####################################
### Check Predictions over years
####################################
meff.plot<-ggeffect(model=mod, c("centred_year [0:35]"))
#exponential to get appraised values
meff.plot<-meff.plot %>%
  mutate(predicted=exp(predicted),
         conf.low=exp(conf.low),
         conf.high=exp(conf.high),
         x=x+1985)

plot(meff.plot)+ 
  xlab("Year")+
  ylab("Appraised Value")+
  ggtitle("Predicted Appraised Value",
    subtitle = "Over 1985-2020 Based on the Model")

####################################
### Check Predictions over years -- Ohio
####################################
meff.plot2<-ggeffect(model=mod, c("centred_year [0:35]", "Ohio"))
#exponential to get appraised values
meff.plot2<-meff.plot2 %>%
  mutate(predicted=exp(predicted),
         conf.low=exp(conf.low),
         conf.high=exp(conf.high),
         x=x+1985,
         group=factor(group, levels = c("n","y"), labels = c("No","Yes")))

plot(meff.plot2)+ 
  xlab("Year")+
  ylab("Appraised Value")+
  ggtitle("Predicted Appraised Value",
          subtitle = "Over 1985-2020 Based on the Model")

####################################
### Check Predictions over years -- Culture
####################################
meff.plot3<-ggeffect(model=mod, c("centred_year [1:35]", "culture"))
#exponential to get appraised values
meff.plot3<-meff.plot3 %>%
  mutate(predicted=exp(predicted),
         conf.low=exp(conf.low),
         conf.high=exp(conf.high),
         x=x+1985)

plot(meff.plot3)+ 
  xlab("Year")+
  ylab("Appraised Value")+
  ggtitle("Predicted Appraised Value",
          subtitle = "Over 1985-2020 Based on the Model")+
  labs(color='Culture') 


####################################
### Check Predictions over years -- Culture + Ohio
####################################
meff.plot4<-ggeffect(model=mod, c("centred_year [0:35]", "culture","Ohio"))

#exponential to get appraised values
meff.plot4<-meff.plot4 %>%
  mutate(predicted=exp(predicted),
         conf.low=exp(conf.low),
         conf.high=exp(conf.high),
         x=x+1985,
         facet=factor(facet, levels = c("n","y"), labels = c("Ohio=No","Ohio=Yes")))

plot(meff.plot4)+ 
  xlab("Year")+
  ylab("Appraised Value")+
  ggtitle("Predicted Appraised Value",
          subtitle = "Over 1985-2020 Based on the Model")+
  labs(color='Culture') 

####################################
### Check Predictions over years -- size
####################################
meff.plot5<-ggeffect(model=mod, c("centred_year [1:35]", "size"))

#exponential to get appraised values
meff.plot5<-meff.plot5 %>%
  mutate(predicted=exp(predicted),
         conf.low=exp(conf.low),
         conf.high=exp(conf.high),
         x=x+1985,
         group=factor(group, levels = c("3.26","5.51","7.75"), 
                      labels = c("Small (3.26)","Medium (5.51)","Large(7.75)")))

plot(meff.plot5)+ 
  xlab("Year")+
  ylab("Appraised Value")+
  ggtitle("Predicted Appraised Value",
          subtitle = "Over 1985-2020 Based on the Model")+
  labs(color='Size') 

####################################
### Check Predictions over years -- size + Ohio
####################################
meff.plot6<-ggeffect(model=mod, c("centred_year [1:35]", "size", "Ohio"))
#exponential to get appraised values
meff.plot6<-meff.plot6 %>%
  mutate(predicted=exp(predicted),
         conf.low=exp(conf.low),
         conf.high=exp(conf.high),
         x=x+1985,
         group=factor(group, levels = c("3.26","5.51","7.75"), 
                      labels = c("Small (3.26)","Medium (5.51)","Large(7.75)")),
         facet=factor(facet, levels = c("n","y"), labels = c("Ohio=No","Ohio=Yes")))

plot(meff.plot6)+ 
  xlab("Year")+
  ylab("Appraised Value")+
  ggtitle("Predicted Appraised Value",
          subtitle = "Over 1985-2020 Based on the Model")+
  labs(color='Size') 

####################################
### Check Predictions over years -- medium
####################################
meff.plot7<-ggeffect(model=mod, c("centred_year [1:35]", "medium"))
#exponential to get appraised values
meff.plot7<-meff.plot7 %>%
  mutate(predicted=exp(predicted),
         conf.low=exp(conf.low),
         conf.high=exp(conf.high),
         x=x+1985,
         group=factor(group, levels = c("bronze","ceramic","faience"), 
                      labels = c("Bronze","Ceramic","Faience")))

plot(meff.plot7)+ 
  xlab("Year")+
  ylab("Appraised Value")+
  ggtitle("Predicted Appraised Value",
          subtitle = "Over 1985-2020 Based on the Model")+
  labs(color='Medium') 


####################################
### Check Predictions over years -- medium + Ohio
####################################
meff.plot8<-ggeffect(model=mod, c("centred_year [1:35]", "medium", "Ohio"))
#exponential to get appraised values
meff.plot8<-meff.plot8 %>%
  mutate(predicted=exp(predicted),
         conf.low=exp(conf.low),
         conf.high=exp(conf.high),
         x=x+1985,
         group=factor(group, levels = c("bronze","ceramic","faience"), 
                      labels = c("Bronze","Ceramic","Faience")),
         facet=factor(facet, levels = c("n","y"), labels = c("Ohio=No","Ohio=Yes")))

plot(meff.plot8)+ 
  xlab("Year")+
  ylab("Appraised Value")+
  ggtitle("Predicted Appraised Value",
          subtitle = "Over 1985-2020 Based on the Model")+
  labs(color='Medium') 


