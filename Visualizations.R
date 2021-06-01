library("tidyverse")
#install.packages("readxl")
library("readxl")
#install.packages("writexl")
library("writexl")
library("patchwork")

## April 4, 2021

#import from excel
analysis2_file<-read_excel("Analysis2.xlsx")

##data cleaning
#bring into tidy format: pivot by date

dat1<-analysis2_file%>%pivot_longer(
  cols = c(starts_with("19"),starts_with("20")), 
  names_to = "year", 
  values_to = "appraised value")

clean_file<-dat1[,c(1:6,11:12)]
clean_file_with_extras<-dat1[,c(1:6,11:12,7:10)]

## visualizations

#(i) listing price histogram: need wider bins
d<- density(clean_file_with_extras$`listing price`, na.rm=T)
ggplot(data=clean_file_with_extras, aes(x=`listing price`)) + #which data to plot
  geom_histogram(aes(y = ..density..), #plots the density
                 binwidth = 2000, #how many bins to use
                 fill = "lightblue", color="black", na.rm=TRUE)  + 
  geom_density(alpha = 0.5,trim=F) +
  xlab("listing price (in $)")  + #x axis label
  ylab("Density")                    + #y axis label
  #ggtitle("Time to death of participants of study", 
  #    subtitle = "Treatment versus control groups") + #add title to plot
  theme_bw()                    + #removes grey background 
  
  geom_hline(yintercept=0)    +      #adds a line for the x-axis
  scale_x_continuous(breaks=c(0,5000,10000,15000,20000))

#(ii) LATER PRICE distribution
d2<- density(clean_file_with_extras$`LATER PRICE`, na.rm=T)
ggplot(data=clean_file_with_extras, aes(x=`LATER PRICE`)) + #which data to plot
  geom_histogram(aes(y = ..density..), #plots the density
                 binwidth = 6000, #how many bins to use
                 fill = "lightblue", color="black", na.rm=TRUE)  + 
  geom_density(alpha = 0.5,trim=F) +
  xlab("Later listing price (in $)")  + #x axis label
  ylab("Density")                    + #y axis label
  #ggtitle("Time to death of participants of study", 
  #    subtitle = "Treatment versus control groups") + #add title to plot
  theme_bw()                    + #removes grey background 
  
  geom_hline(yintercept=0)    +      #adds a line for the x-axis
  scale_x_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000))

#(iii) scatter plot: listing price versus later listing price
ggplot(clean_file_with_extras, aes(x=`listing price`, y=`LATER PRICE`)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+
  xlab("Listing price (in $)")  + #x axis label
  ylab("Later listing price (in $)") +   #y axis label
  theme_bw() +
  ggtitle("Scatter plot: Later listing price versus listing price")

#####Total change in valuation over time (in $)
temp_file<-analysis2_file
temp_file<-temp_file%>% mutate(val_diff_total = analysis2_file$`LATER PRICE`- analysis2_file$`listing price`)

d3<- density(temp_file$`val_diff_total`, na.rm=T)
valuation_change<-ggplot(data=temp_file, aes(x=`val_diff_total`)) + #which data to plot
  geom_histogram(aes(y = ..density..), #plots the density
                 binwidth = d3$bw, #how many bins to use
                 fill = "lightblue", color="black", na.rm=TRUE)  + 
  geom_density(alpha = 0.5,trim=F) +
  xlab("Total change in valuation over time (in $)")  + #x axis label
  ylab("Density")                    + #y axis label
  ggtitle("Total change in valuation over time (in $)")+
  #    subtitle = "Treatment versus control groups") + #add title to plot
  theme_bw()                    + #removes grey background 
  
  geom_hline(yintercept=0)    +      #adds a line for the x-axis
  scale_x_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000,35000,40000,45000,50000))+
 theme(axis.text.x = element_text(angle = 90))

#create variable to store %return on items
newvars_file<-analysis2_file
newvars_file<-newvars_file%>%mutate(percent_return=(`LATER PRICE`-`listing price`)*100/`listing price`)
#(iv) plot a histogram of %return on items so it gives a clearer picture:
d3<- density(newvars_file$`percent_return`, na.rm=T)
percent_return<-ggplot(data=newvars_file, aes(x=`percent_return`)) + #which data to plot
  geom_histogram(aes(y = ..density..), #plots the density
                 binwidth = d3$bw, #how many bins to use
                 fill = "lightblue", color="black", na.rm=TRUE)  + 
  geom_density(alpha = 0.5,trim=F) +
  xlab("Percentage return on items (in %)")  + #x axis label
  ylab("Density")                    + #y axis label
  scale_x_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450))+
  ggtitle("Percentage return on items (in %)")+#add title to plot
  theme_bw()                    + #removes grey background 
  
  geom_hline(yintercept=0) +       #adds a line for the x-axis
  theme(axis.text.x = element_text(angle = 90))
valuation_change+percent_return
  
# (v) try to tie listing price to later listing price

# (vi) What is the distribution of first year listed?
fy_listed<-ggplot(data=analysis2_file, aes(x=`year of listing`)) + #which data to plot
  
  geom_bar( fill = "lightblue", color="black", na.rm=TRUE,stat="count")  + 
  xlab("First year listed")  + #x axis label
  ylab("Count")                    + #y axis label
  theme_bw()                    + #removes grey background 
  geom_hline(yintercept=0)+
  scale_x_continuous(breaks=1984:1993)+
  ggtitle("First year listed")+
  theme(axis.text.x = element_text(angle = 90))


# (vii) What is the distribution of last year listed (titled LATER LISTING YEAR in the dataset)?
ly_listed<-ggplot(data=analysis2_file, aes(x=`LATER  LISTING YEAR`)) + #which data to plot
  geom_bar( fill = "lightblue", color="black", na.rm=TRUE,stat="count")  + 
  xlab("Last year listed")  + #x axis label
  ylab("Count")                    + #y axis label
  theme_bw()                    + #removes grey background 
  geom_hline(yintercept=0)+
  scale_x_continuous(breaks=seq(1997,2020,by=1))+
  ggtitle("Last year listed")+
  theme(axis.text.x = element_text(angle = 90))

fy_listed+ly_listed

# (vii) Mean appraised value over the years:Points and path
mean_appraised_val_by_year<-read_csv('mean_appraised_val_by_year.csv')
ggplot(data = mean_appraised_val_by_year,aes(x=year, y=mean))+
geom_point()+
  theme_bw()                    + #removes grey background 
  geom_hline(yintercept=0)+
  scale_x_continuous(breaks=seq(1985,2020,by=1))+
  scale_y_continuous(breaks=seq(0,30000,by=5000))+
  ggtitle("Changes in Mean Appraised Value of Antiquities from 1985 to 2020")+geom_path()+
  xlab("Year of appraisal")  + #x axis label
  ylab("Mean appriased value of antiquities (in $)") +
  theme(axis.text.x = element_text(angle = 90))

# (viii)Mean appraised value over the years:Only path
mean_appraised_val_by_year<-read_csv('mean_appraised_val_by_year.csv')
ggplot(data = mean_appraised_val_by_year,aes(x=year, y=mean))+
geom_path()+
  theme_bw()                    + #removes grey background 
  geom_hline(yintercept=0)+
  scale_x_continuous(breaks=seq(1985,2020,by=3))+
  scale_y_continuous(breaks=seq(0,30000,by=5000))+
  ggtitle("Changes in Mean Appraised Value of Antiquities from 1985 to 2020")+
  xlab("Year of appraisal")  + #x axis label
  ylab("Mean appriased value of antiquities (in $)")

# (ix) Average appraised value by Ohio:
clean_file_with_extras_temp<-clean_file_with_extras
#Need to group antiquities by Ohio first, then calculate average appraised value by year
Mean_appraised_value_Ohio_n<-clean_file_with_extras_temp%>%filter(`Ohio?`=="n")%>%drop_na(`appraised value`)%>%group_by(year)%>%summarize(mean_appr_val_Ohio_n=mean(`appraised value`))
Mean_appraised_value_Ohio_y<-clean_file_with_extras_temp%>%filter(`Ohio?`=="y")%>%drop_na(`appraised value`)%>%group_by(year)%>%summarize(mean_appr_val_Ohio_y=mean(`appraised value`))

# #group by --> factor
# #Emma's example for grouping
# datHel$Colgate_Id <- factor(datHel$Colgate_Id, levels=as.character(unique(datHel$Colgate_Id)))
# p7<-ggplot(datHel, aes(x=Year, y = Price,group = Ohio, color=Ohio))+
#   geom_line()+
#   theme_bw()+
#   ggtitle("Changes in Mean Appraised Value of Antiquities from 1985 to 2020", subtitle="Grouped by whether they went to Ohio or not")+
#   theme(plot.title=element_text(face="bold", size=15))+
#   ylab("Mean Appraised Value of Antiquities ($")+
#   xlab("year")

#box plots by group
Mean_appraised_value_Ohio_n<-clean_file_with_extras_temp%>%filter(`Ohio?`=="n")%>%drop_na(`appraised value`)%>%group_by(year)%>%summarize(mean_appr_val_Ohio_n=mean(`appraised value`))
Mean_appraised_value_Ohio_y<-clean_file_with_extras_temp%>%filter(`Ohio?`=="y")%>%drop_na(`appraised value`)%>%group_by(year)%>%summarize(mean_appr_val_Ohio_y=mean(`appraised value`))
d1<-merge(x=Mean_appraised_value_Ohio_n,y=Mean_appraised_value_Ohio_y, by="year")

# d2<-d1%>%pivot_longer(
#   cols = c(all_of(Mean_appraised_value_Ohio_n),all_of(Mean_appraised_value_Ohio_y)), 
#   names_to = "category", 
#   values_to = "mean appraised value")

d1$year<-as.numeric(d1$year)
colors <- c("Went to Ohio" = "steelblue","Did not go to Ohio" = "darkred")
ggplot(d1, aes(x=year,group=1))+
  geom_point(aes(y = mean_appr_val_Ohio_y,color="Went to Ohio"))+
  geom_path(aes(y = mean_appr_val_Ohio_y,color="Went to Ohio"))+
  geom_point(aes(y = mean_appr_val_Ohio_n,color="Did not go to Ohio"))+
  geom_path(aes(y = mean_appr_val_Ohio_n,color="Did not go to Ohio"))+
  theme_bw()+
  ggtitle("Changes in Mean Appraised Value of Antiquities from 1985 to 2011",
          subtitle="Grouped by whether they went to Ohio or not")+
  labs(x="Year",
       y="Mean Appraised Value of Antiquities ($)",
       color="Legend")+scale_color_manual(values = colors)+
  scale_x_continuous(breaks=seq(1985,2020,by=1))+
  theme(axis.text.x = element_text(angle = 90))

# (x) Average appraised value by Culture:

d_culture<-clean_file_with_extras%>%drop_na(`appraised value`)%>%group_by(culture,year)%>%summarize(mean=mean(`appraised value`))
d_culture<-d_culture%>%rename(Culture=culture)
d_culture$year<-as.numeric(d_culture$year)
ggplot(d_culture, aes(x=year, y = mean,group = Culture))+
   geom_line(aes(color=Culture))+
   theme_bw()+
   ggtitle("Changes in Mean Appraised Value of Antiquities from 1985 to 2011", subtitle="Grouped by Culture")+
   theme(plot.title=element_text(face="bold", size=15))+
   ylab("Mean Appraised Value of Antiquities ($)")+
   xlab("year")+
   scale_x_continuous(breaks=seq(1985,2020,by=1))+
   theme(axis.text.x = element_text(angle = 90))

# (xi) Average appraised value by Medium:
d_medium<-clean_file_with_extras%>%drop_na(`appraised value`)%>%group_by(medium,year)%>%summarize(mean=mean(`appraised value`))
d_medium<-d_medium%>%rename(Medium=medium)
d_medium$year<-as.numeric(d_medium$year)
  ggplot(d_medium, aes(x=year, y = mean,group = Medium))+
  geom_line(aes(color=Medium))+
  theme_bw()+
  ggtitle("Changes in Mean Appraised Value of Antiquities from 1985 to 2011", subtitle="Grouped by Medium")+
  theme(plot.title=element_text(face="bold", size=15))+
  ylab("Mean Appraised Value of Antiquities ($)")+
  xlab("year")+
  scale_x_continuous(breaks=seq(1985,2020,by=1))+
  theme(axis.text.x = element_text(angle = 90))

  #Size distribution
  dsize<- density(clean_file_with_extras$`size (in inches)`, na.rm=T)
  ggplot(data=clean_file_with_extras, aes(x=`size (in inches)`)) + #which data to plot
    geom_histogram(aes(y = ..density..), #plots the density
                   binwidth = 1.2, #how many bins to use
                   fill = "lightblue", color="black", na.rm=TRUE)  + 
    geom_density(alpha = 0.5,trim=F) +
    xlab("Size (in inches)")  + #x axis label
    ylab("Density")                    + #y axis label
    ggtitle("Histogram: Size of Antiquities") + #add title to plot
    theme_bw()                    + #removes grey background 
    geom_hline(yintercept=0)    #adds a line for the x-axis
    #scale_x_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000))
  
  # (xii) Average appraised value by Size:
  years<-data.frame(year=1985:2020)
  clean_file_with_extras_temp<-merge(clean_file_with_extras_temp,years,by="year",all=TRUE)
  clean_file_with_extras_temp<-
    clean_file_with_extras_temp%>%drop_na(`size (in inches)`)%>%mutate(size_cat=
                                           ifelse(`size (in inches)`<=5,
                                           "less than or equal to 5 inches",
                                         ifelse(`size (in inches)`<=10,
                                            "between 5 and 10 inches",
                                            "greater than 10 inches")))
  d_size<-clean_file_with_extras_temp%>%drop_na(`appraised value`)%>%group_by(size_cat,year)%>%summarize(mean=mean(`appraised value`))
  d_size<-d_size%>%rename(Size=size_cat)
  d_size$Size<-factor(d_size$Size, levels = c("less than or equal to 5 inches",
                                              "between 5 and 10 inches",
                                              "greater than 10 inches"))
  

  
  ggplot(d_size, aes(x=year, y = mean,group = Size))+
    geom_line(aes(color=Size))+
    theme_bw()+
    ggtitle("Changes in Mean Appraised Value of Antiquities from 1985 to 2020", subtitle="Grouped by Size Category")+
    theme(plot.title=element_text(face="bold", size=15))+
    ylab("Mean Appraised Value of Antiquities ($)")+
    xlab("year")+
    scale_x_discrete(breaks=seq(1985,2020,by=2))
