#Install packages
install.packages("tidyverse")

#Load libraries
library(tidyverse)

#Import & read data
df_src<-read_csv("D:/Analytics/UCF/Data_Visualization/Assignment/DV_Assign05/budgets.csv")
head(df_src)
str(df_src)

#####
#1 - Diverging Bar Charts
#####

#Data Preparation
df1 <- df_src 
df1$pctdev <- (df1$'2021' - df1$'2020')/df1$'2020'
df1$dev_type <- ifelse(df1$pctdev < 0, "Negative", "Positive")  # above / below dev flag
df1$category <- factor(df1$category, levels = df1$category)  # convert to factor to retain sorted order in plot.

df1
df1$pctdev

# Plot charts
ggplot(df1, aes(x=category, y=pctdev, label=pctdev)) + 
  geom_bar(stat='identity', aes(fill=dev_type), width=.5)  +
  scale_fill_manual(name="Budget", 
                    labels = c("Negative Deviation", "Positive Deviation"), 
                    values = c("Negative"="#f8766d", "Positive"="#00ba38")) + 
  labs(x="Cateogy",y="Budget Deviation", subtitle="Budget Deviation between 2021 and 2020", 
       title= "Diverging Bars") + 
  coord_flip() 


#####
#2 - Grouped Bar Charts
#####

# Data Preparation
df2_1 <- df_src%>%gather(key="year",
                           value="budget",'2019','2020','2021','2022',
                           '2023','2024','2025','2026','2027','2028',
                           '2029','2030') #Convert wide to long format

df2_1

df2_2 <- df2_1 %>%
  filter(!year %in% c('2019','2020')) %>% 
  group_by(programs, year) %>% 
  summarize(budget= sum(budget,na.rm=TRUE)) # Filter, group and summarize data

df2_2

#ABOVE CODE CAN ALSO BE WRITTEN IN SINGLE LINE
#df2_2 <- 1%>%gather(key="year",
#                           value="budget",'2019','2020','2021','2022',
#                           '2023','2024','2025','2026','2027','2028',
#                           '2029','2030') %>%
#               filter(!year %in% c('2019','2020')) %>% 
#               group_by(programs, year) %>% 
#               summarize(budget= sum(budget,na.rm=TRUE))
#df2_2

#Comparison chart 
ggplot(df2_2, aes(fill=programs,y=budget, x=year)) + 
  geom_bar(position="dodge", stat="identity")  +
  labs(x="Year",y="Budget", title= "Proposed Budget Spending from the year 2021  to 2030")
