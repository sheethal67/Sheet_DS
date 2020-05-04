#Install packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggthemes")

#Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)

#Import & read data
df_src<-read.csv("D:/Analyitcs/UCF/Assignment/Approval_Topline.csv")
df_src
head(df_src)
str(df_src)
names(df_src)

#Change date format as a new variable
df_test <- mutate(df_src, model_date = as.Date(modeldate, format = "%m/%d/%Y"))
head(df_test)

#Filter data sets
Voters <- filter(df_test, subgroup == 'Voters')
Adults <- filter(df_test, subgroup == 'Adults')
All_Polls <- filter(df_test, subgroup == 'All polls')

#Read Filtered data
head(Voters)
head(Adults)
head(All_Polls)

#Extract sequence of 6 weeks intervals
x1 <- seq(from = max(Voters$model_date), to = min(Voters$model_date), by='-6 weeks')
x1

x2 <- seq(from = max(Adults$model_date), to = min(Adults$model_date), by='-6 weeks')
x2

x3 <- seq(from = max(All_Polls$model_date), to = min(All_Polls$model_date), by='-6 weeks')
x3

#Extract subset of data with 6 week intervals
week_intervals1 <- Voters[which(Voters$model_date %in% x1),]
week_intervals1

week_intervals2 <- Adults[which(Adults$model_date %in% x2),]
week_intervals2

week_intervals3 <- All_Polls[which(All_Polls$model_date %in% x3),]
week_intervals3


#Generate required plot for Voters
Voter_plot <- ggplot(Voters) +
  geom_line(aes(y=disapprove_estimate, x=model_date, color = "red")) +
  geom_ribbon(aes(ymin=disapprove_lo, ymax=disapprove_hi, x=model_date, fill = "band"), alpha = 0.3) +  theme(legend.position = "none")+
  geom_pointrange(data=week_intervals1, aes(x=model_date, y=disapprove_estimate, ymin=disapprove_lo, ymax=disapprove_hi), color="red") +
  geom_line(aes(y=approve_estimate, x=model_date, color = "darkgreen")) +
  geom_ribbon(aes(ymin=approve_lo, ymax=approve_hi, x=model_date, fill = "darkgreen"), alpha = 0.3) +  theme(legend.position = "none")+
  geom_pointrange(data=week_intervals1, aes(x=model_date, y=approve_estimate, ymin=approve_lo, ymax=approve_hi), color="darkgreen") +
  ggtitle("President's approval and disapproval from Voters") +
  xlab("Year") +
  ylab("Estimate") 

#Value Representation for Voters
Avlast=Voters[Voters$model_date == max(Voters$model_date),"approve_estimate"]
Dvlast=Voters[Voters$model_date == max(Voters$model_date),"disapprove_estimate"]
Dvround=round(Dvlast,digits=1)
Avround=round(Avlast,digits=1)
Dvlabel=paste(Dround,"% Disapprove")
Avlabel=paste(Around,"% Approve")

#Final Plot for Voters Data
Voter_plot + annotation_custom(grob=textGrob(label=Avlabel, hjust=0, gp=gpar(cex=1)),
                               ymin=Avlast,ymax=Avlast,xmin=max(Voters$model_date)+days(20), xmax=max(Voters$model_date)+days(20)) +
  annotation_custom(grob=textGrob(label=Dvlabel, hjust=0, gp=gpar(cex=1)),
                    ymin=Dvlast,ymax=Dvlast,xmin=max(Voters$model_date)+days(20), xmax=max(Voters$model_date)+days(20))


#Generate required plot for Adults
Adults_plot <- ggplot(Adults) +
  geom_line(aes(y=disapprove_estimate, x=model_date, color = "red")) +
  geom_ribbon(aes(ymin=disapprove_lo, ymax=disapprove_hi, x=model_date, fill = "band"), alpha = 0.3) + theme(legend.position = "none")+
  geom_pointrange(data=week_intervals2, aes(x=model_date, y=disapprove_estimate, ymin=disapprove_lo, ymax=disapprove_hi), color="red") +
  geom_line(aes(y=approve_estimate, x=model_date, color = "darkgreen")) +
  geom_ribbon(aes(ymin=approve_lo, ymax=approve_hi, x=model_date, fill = "darkgreen"), alpha = 0.3) + theme(legend.position = "none")+
  geom_pointrange(data=week_intervals2, aes(x=model_date, y=approve_estimate, ymin=approve_lo, ymax=approve_hi), color="darkgreen") +
  ggtitle("President's approval and disapproval from Adults") +
  xlab("Year") +
  ylab("Estimate")

#Value Representation for Adults
Aalast=Adults[Adults$model_date == max(Adults$model_date),"approve_estimate"]
Dalast=Adults[Adults$model_date == max(Adults$model_date),"disapprove_estimate"]
Daround=round(Dalast,digits=1)
Aaround=round(Aalast,digits=1)
Dalabel=paste(Daround,"% Disapprove")
Aalabel=paste(Aaround,"% Approve")

#Final Plot for Adults Data
Adults_plot + annotation_custom(grob=textGrob(label=Aalabel, hjust=0, gp=gpar(cex=1)),
                                ymin=Aalast,ymax=Aalast,xmin=max(Adults$model_date)+days(20), xmax=max(Adults$model_date)+days(20)) +
  annotation_custom(grob=textGrob(label=Dalabel, hjust=0, gp=gpar(cex=1)),
                    ymin=Dalast,ymax=Dalast,xmin=max(Adults$model_date)+days(20), xmax=max(Adults$model_date)+days(20))


#Generate required plot for All Polls
Allpolls_plot <- ggplot(All_Polls) +
  geom_line(aes(y=disapprove_estimate, x=model_date, color = "red")) +
  geom_ribbon(aes(ymin=disapprove_lo, ymax=disapprove_hi, x=model_date, fill = "band"), alpha = 0.3) + theme(legend.position = "none")+
  geom_pointrange(data=week_intervals3, aes(x=model_date, y=disapprove_estimate, ymin=disapprove_lo, ymax=disapprove_hi), color="red") +
  geom_line(aes(y=approve_estimate, x=model_date, color = "darkgreen")) +
  geom_ribbon(aes(ymin=approve_lo, ymax=approve_hi, x=model_date, fill = "darkgreen"), alpha = 0.3) + theme(legend.position = "none")+
  geom_pointrange(data=week_intervals3, aes(x=model_date, y=approve_estimate, ymin=approve_lo, ymax=approve_hi), color="darkgreen") +
  ggtitle("President's approval and disapproval from All polls") +
  xlab("Year") +
  ylab("Estimate") 

#Value Representation for All Polls
Aplast=All_Polls[All_Polls$model_date == max(All_Polls$model_date),"approve_estimate"]
Dplast=All_Polls[All_Polls$model_date == max(All_Polls$model_date),"disapprove_estimate"]
Dpround=round(Dplast,digits=1)
Apround=round(Aplast,digits=1)
Dplabel=paste(Dpround,"% Disapprove")
Aplabel=paste(Apround,"% Approve") 

#Final Plot for All Polls Data
Allpolls_plot + annotation_custom(grob=textGrob(label=Aplabel, hjust=0, gp=gpar(cex=1)),
                                  ymin=Aplast,ymax=Aplast,xmin=max(All_Polls$model_date)+days(20), xmax=max(All_Polls$model_date)+days(20)) +
  annotation_custom(grob=textGrob(label=Dplabel, hjust=0, gp=gpar(cex=1)),ymin=Dplast,
                    ymax=Dplast,xmin=max(All_Polls$model_date)+days(20), xmax=max(All_Polls$model_date)+days(20))


