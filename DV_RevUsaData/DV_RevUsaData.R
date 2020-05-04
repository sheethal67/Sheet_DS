#Install packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggthemes")
install.packages("zeallot")
install.packages("OECD")
install.packages("sqldf")


#Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)
library(zeallot)
library(OECD)
library(sqldf)


#Import & read data
df_src<-get_dataset("REVUSA")

write.csv(df_src,"D:/Analyitcs/UCF/1.Monday - Interactive_Data_Visualization/Assignment/DV_Assign03/RevUsaData.csv")

head(df_src)


#Filter for list of taxes to analyze
sqldf("SELECT distinct TAX FROM df_src")

subset1<-c("TOTALTAX","1000","2000","3000","4000","5000","6000")
subset1

df0 <- filter(df_src, TAX %in% subset1)

sqldf("SELECT distinct TAX FROM df0")


#Convert string to date format
str(df0)

df1 <- mutate (df0, obsTime = as.Date(obsTime,"%Y"))

str(df1)


#Rename variables in subset1
head(df1)

levelKeys = c("TOTALTAX" = "Total Tax", "1000" = "Corporate Income Tax", "2000" = "Social Security Tax", "3000"= "Payroll Tax", "4000" = "Property Tax", "5000" = "Taxes on Service and Goods","6000" = "Other Taxes")
df2 <- mutate(df1,TAX=recode(TAX, !!!levelKeys))

head(df2)


#1 - Create a line plot of Total Tax as a function time
df2_1 <- filter(df2, TAX == 'Total Tax')
ggplot(data = df2_1, aes(obsTime,obsValue)) + geom_line()

sqldf("SELECT distinct GOV FROM df2_1")
ggplot(data = df2_1, aes(obsTime,obsValue)) + geom_line() + facet_wrap(~ GOV)

#df2_1_FED <- filter(df2, TAX == 'Total Tax' & GOV == 'FED')
#ggplot(data = df2_1_FED, aes(obsTime,obsValue)) + geom_line()

#ggplot(data = df2_1, aes(Year,Value)) + geom_line() + scale_x_date(date_breaks="5")


#2 - Create multiline plots of all the different data (except "Total Tax") as a function of time
df2_2 <- filter(df2, TAX != 'Total Tax')
ggplot(df2_2, aes(x=obsTime, y=obsValue, color=TAX)) + geom_line()

ggplot(df2_2, aes(x=obsTime, y=obsValue, color=TAX)) + geom_line() + facet_wrap(~ GOV) + theme(legend.position = "bottom")


#3 - stacked area plots to show all the different data (except "Total Tax") as a function of time.
ggplot(df2_2, aes(x=obsTime, y=obsValue, fill=TAX)) + geom_area()

ggplot(df2_2, aes(x=obsTime, y=obsValue, fill=TAX)) + geom_area() + facet_wrap(~ GOV) + theme(legend.position = "bottom")


#4 - Create a proportional stacked area plot by computing the percentage of individual taxes with respect to Total tax and plotting stacked area plot of those percentages as a function of time.
df3 <- sqldf("SELECT t1.TAX as TAX_tot
                , t1.GOV as GOV_tot
                , t1.TIME_FORMAT as TIME_FORMAT_tot
                , t1.obsTime as obsTime_tot
                , t1.obsValue as obsValue_tot
                , (100*t2.obsValue)/t1.obsValue as Percent
                , t2.*
             FROM df2_1 t1
             INNER JOIN df2_2 t2 ON (t1.obsTime=t2.obsTime AND t1.GOV=t2.GOV)
             ")

head(df3)

ggplot(df3, aes(x=obsTime, y=Percent, fill=TAX)) + geom_area()

ggplot(df3, aes(x=obsTime, y=Percent, fill=TAX)) + geom_area() + facet_wrap(~ GOV) + theme(legend.position = "bottom")


