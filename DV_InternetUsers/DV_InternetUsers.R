#Install packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggthemes")
install.packages("zeallot")
install.packages("sqldf")
install.packages("readxl")


#Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)
library(zeallot)
library(sqldf)
library(readxl)


#Import & read data
df_src<-read_excel("..../Assignment4_Data.xlsx")
head(df_src)

#Convert string to date format
str(df_src)

#df1 <- mutate (df_src, obsYear = as.Date(as.character(obsYear),"%Y"))
df1 <- mutate (df_src, obsYear = as.character(obsYear))

str(df1)

#Filter for list of taxes to analyze
sqldf("SELECT distinct Category1 FROM df1")

df_Development <- filter(df1, Category1 == 'Development')
df_Region <- filter(df1, Category1 == 'Region')

sqldf("SELECT distinct Indicator FROM df_Region")

#####
#1 - Visualization: Fixed-telephone subscriptions
#####

#Comparison chart 
ggplot(filter(df_Region, Indicator == "Fixed-telephone subscriptions"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(position="dodge",stat="identity")  + coord_flip()+
  labs(title ="Grouped Bar Chart For Fixed-telephone subscriptions ")

#Region wise ranking
ggplot(filter(df_Region, Indicator == "Fixed-telephone subscriptions"),
       aes(y=obsValue, x=Category2)) +
  geom_segment(aes(xend = Category2, yend = 0)) + 
  facet_wrap(~ obsYear, scales="free") +
  geom_point(size=5, color = "blue")  + coord_flip() +
  labs(title="Lollipop Chart For Fixed-telephone subscriptions ")

#Pie chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Fixed-telephone subscriptions"),
       aes(fill=Category2, y=obsValue, x="")) + 
  geom_bar(position="fill",stat = "identity", color="white") +
  facet_wrap(~ obsYear) +
  theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, y=NULL, title = "Pie Chart For Fixed-telephone subscriptions", caption="Millions") +
  coord_polar(theta = "y", start=0) 

#Stacked bar chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Fixed-telephone subscriptions"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(stat="identity")  + coord_flip() +
  labs(title = "Stacked Bar Chart For Fixed-telephone subscriptions")



#####
#2 - Visualization: Mobile-cellular telephone subscriptions
#####

#Comparison chart 
ggplot(filter(df_Region, Indicator == "Mobile-cellular telephone subscriptions"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(position="dodge",stat="identity")  + coord_flip()+
  labs(title ="Grouped Bar Chart For Mobile-cellular telephone subscriptions")

#Region wise ranking
ggplot(filter(df_Region, Indicator == "Mobile-cellular telephone subscriptions"),
       aes(y=obsValue, x=Category2)) +
  geom_segment(aes(xend = Category2, yend = 0)) + 
  facet_wrap(~ obsYear, scales="free") +
  geom_point(size=5, color = "blue")  + coord_flip()+
  labs(title="Lollipop Chart For Mobile-cellular telephone subscriptions")

#Pie chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Mobile-cellular telephone subscriptions"),
       aes(fill=Category2, y=obsValue, x="")) + 
  geom_bar(position="fill",stat = "identity", color="white") +
  facet_wrap(~ obsYear) +
  theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, y=NULL, title = "Pie Chart For Mobile-cellular telephone subscriptions", caption="Millions") +
  coord_polar(theta = "y", start=0) 

#Stacked bar chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Mobile-cellular telephone subscriptions"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(stat="identity")  + coord_flip()+
  labs(title = "Stacked Bar Chart For Mobile-cellular telephone subscriptions")



#####
#3 - Visualization: Active mobile-broadband subscriptions
#####

#Comparison chart 
ggplot(filter(df_Region, Indicator == "Active mobile-broadband subscriptions"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(position="dodge",stat="identity")  + coord_flip()+
  labs(title ="Grouped Bar Chart For Active mobile-broadband subscriptions")

#Region wise ranking
ggplot(filter(df_Region, Indicator == "Active mobile-broadband subscriptions"),
       aes(y=obsValue, x=Category2)) +
  geom_segment(aes(xend = Category2, yend = 0)) + 
  facet_wrap(~ obsYear, scales="free") +
  geom_point(size=5, color = "blue")  + coord_flip()+
  labs(title="Lollipop Chart For Active mobile-broadband subscriptions")

#Pie chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Active mobile-broadband subscriptions"),
       aes(fill=Category2, y=obsValue, x="")) + 
  geom_bar(position="fill",stat = "identity", color="white") +
  facet_wrap(~ obsYear) +
  theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, y=NULL,title = "Pie Chart For Active mobile-broadband subscriptions", caption="Millions") +
  coord_polar(theta = "y", start=0) 

#Stacked bar chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Active mobile-broadband subscriptions"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(stat="identity")  + coord_flip()+
  labs(title = "Stacked Bar Chart For Active mobile-broadband subscriptions")


#####
#4 - Visualization: Fixed-broadband subscriptions
#####

#Comparison chart 
ggplot(filter(df_Region, Indicator == "Fixed-broadband subscriptions"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(position="dodge",stat="identity")  + coord_flip()+
  labs(title ="Grouped Bar Chart For Fixed-broadband subscriptions")

#Region wise ranking
ggplot(filter(df_Region, Indicator == "Fixed-broadband subscriptions"),
       aes(y=obsValue, x=Category2)) +
  geom_segment(aes(xend = Category2, yend = 0)) + 
  facet_wrap(~ obsYear, scales="free") +
  geom_point(size=5, color = "blue")  + coord_flip()+
  labs(title="Lollipop Chart For Fixed-broadband subscriptions")

#Pie chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Fixed-broadband subscriptions"),
       aes(fill=Category2, y=obsValue, x="")) + 
  geom_bar(position="fill",stat = "identity", color="white") +
  facet_wrap(~ obsYear) +
  theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, y=NULL, title = "Pie Chart For Fixed-broadband subscriptions", caption="Millions") +
  coord_polar(theta = "y",start=0) 

#Stacked bar chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Fixed-broadband subscriptions"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(stat="identity")  + coord_flip()+
  labs(title = "Stacked Bar Chart For Fixed-broadband subscriptions")



#####
#5 - Visualization: Population covered by a mobile-cellular network
#####

#Comparison chart 
ggplot(filter(df_Region, Indicator == "Population covered by a mobile-cellular network"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(position="dodge",stat="identity")  + coord_flip()+
  labs(title ="Grouped Bar Chart For Population covered by a mobile-cellular network")

#Region wise ranking
ggplot(filter(df_Region, Indicator == "Population covered by a mobile-cellular network"),
       aes(y=obsValue, x=Category2)) +
  geom_segment(aes(xend = Category2, yend = 0)) + 
  facet_wrap(~ obsYear, scales="free") +
  geom_point(size=5, color = "blue")  + coord_flip()+
  labs(title="Lollipop Chart For Population covered by a mobile-cellular network")

#Pie chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Population covered by a mobile-cellular network"),
       aes(fill=Category2, y=obsValue, x="")) + 
  geom_bar(position="fill",stat = "identity", color="white") +
  facet_wrap(~ obsYear) +
  theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, y=NULL,title = "Pie Chart For Population covered by a mobile-cellular network", caption="Millions") +
  coord_polar(theta = "y",  start=0) 

#Stacked bar chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Population covered by a mobile-cellular network"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(stat="identity")  + coord_flip()+
  labs(title = "Stacked Bar Chart For Population covered by a mobile-cellular network")



#####
#6 - Visualization: Population covered by at least a 3G mobile network
#####

#Comparison chart 
ggplot(filter(df_Region, Indicator == "Population covered by at least a 3G mobile network"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(position="dodge",stat="identity")  + coord_flip()+
  labs(title ="Grouped Bar Chart For Population covered by at least a 3G mobile network")

#Region wise ranking
ggplot(filter(df_Region, Indicator == "Population covered by at least a 3G mobile network"),
       aes(y=obsValue, x=Category2)) +
  geom_segment(aes(xend = Category2, yend = 0)) + 
  facet_wrap(~ obsYear, scales="free") +
  geom_point(size=5, color = "blue")  + coord_flip()+
  labs(title="Lollipop Chart For Population covered by at least a 3G mobile network")

#Pie chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Population covered by at least a 3G mobile network"),
       aes(fill=Category2, y=obsValue, x="")) + 
  geom_bar(position="fill",stat = "identity", color="white") +
  facet_wrap(~ obsYear) +
  theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, y=NULL,title = "Pie Chart For Population covered by at least a 3G mobile network", caption="Millions") +
  coord_polar(theta = "y", start=0) 

#Stacked bar chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Population covered by at least a 3G mobile network"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(stat="identity")  + coord_flip()+
  labs(title = "Stacked Bar Chart For Population covered by at least a 3G mobile network")



#####
#7 - Visualization: Population covered by at least an LTE/WiMAX mobile network
#####

#Comparison chart 
ggplot(filter(df_Region, Indicator == "Population covered by at least an LTE/WiMAX mobile network"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(position="dodge",stat="identity")  + coord_flip()+
  labs(title ="Grouped Bar Chart For Population covered by at least an LTE/WiMAX mobile network")

#Region wise ranking
ggplot(filter(df_Region, Indicator == "Population covered by at least an LTE/WiMAX mobile network"),
       aes(y=obsValue, x=Category2)) +
  geom_segment(aes(xend = Category2, yend = 0)) + 
  facet_wrap(~ obsYear, scales="free") +
  geom_point(size=5, color = "blue")  + coord_flip()+
  labs(title="Lollipop Chart For Population covered by at least an LTE/WiMAX mobile network")

#Pie chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Population covered by at least an LTE/WiMAX mobile network"),
       aes(fill=Category2, y=obsValue, x="")) + 
  geom_bar(position="fill",stat = "identity", color="white") +
  facet_wrap(~ obsYear) +
  theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, y=NULL,title = "Pie Chart For Population covered by at least an LTE/WiMAX mobile network", caption="Millions") +
  coord_polar(theta = "y", start=0) 

#Stacked bar chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Population covered by at least an LTE/WiMAX mobile network"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(stat="identity")  + coord_flip()+
  labs(title = "Stacked Bar Chart For Population covered by at least an LTE/WiMAX mobile network")



#####
#8 - Visualization: Individuals using the Internet
#####

#Comparison chart 
ggplot(filter(df_Region, Indicator == "Individuals using the Internet"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(position="dodge",stat="identity")  + coord_flip()+
  labs(title ="Grouped Bar Chart For Individuals using the Internet")

#Region wise ranking
ggplot(filter(df_Region, Indicator == "Individuals using the Internet"),
       aes(y=obsValue, x=Category2)) +
  geom_segment(aes(xend = Category2, yend = 0)) + 
  facet_wrap(~ obsYear, scales="free") +
  geom_point(size=5, color = "blue")  + coord_flip()+
  labs(title="Lollipop Chart For Individuals using the Internet")

#Pie chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Individuals using the Internet"),
       aes(fill=Category2, y=obsValue, x="")) + 
  geom_bar(position="fill",stat = "identity", color="white") +
  facet_wrap(~ obsYear) +
  theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, y=NULL,title = "Pie Chart For Individuals using the Internet", caption="Millions") +
  coord_polar(theta = "y", start=0) 

#Stacked bar chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "Individuals using the Internet"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(stat="identity")  + coord_flip()+
  labs(title = "Stacked Bar Chart For Individuals using the Internet")


#####
#9 - Visualization: International bandwidth, in Gbit/s
#####

#Comparison chart 
ggplot(filter(df_Region, Indicator == "International bandwidth, in Gbit/s"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(position="dodge",stat="identity")  + coord_flip()+
  labs(title ="Grouped Bar Chart For International bandwidth, in Gbit/s ")

#Region wise ranking
ggplot(filter(df_Region, Indicator == "International bandwidth, in Gbit/s"),
       aes(y=obsValue, x=Category2)) +
  geom_segment(aes(xend = Category2, yend = 0)) + 
  facet_wrap(~ obsYear, scales="free") +
  geom_point(size=5, color = "blue")  + coord_flip()+
  labs(title="Lollipop Chart For International bandwidth, in Gbit/s")

#Pie chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "International bandwidth, in Gbit/s"),
       aes(fill=Category2, y=obsValue, x="")) + 
  geom_bar(position="fill",stat = "identity", color="white") +
  facet_wrap(~ obsYear) +
  theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, y=NULL,title = "Pie Chart For International bandwidth, in Gbit/s", caption="Millions") +
  coord_polar(theta = "y", start=0) 

#Stacked bar chart for part in whole relationship
ggplot(filter(df_Region, Indicator == "International bandwidth, in Gbit/s"),
       aes(fill=Category2, y=obsValue, x=obsYear)) +
  geom_bar(stat="identity")  + coord_flip()+
  labs(title = "Stacked Bar Chart For International bandwidth, in Gbit/s")

