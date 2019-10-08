#The variables included in this dataset are:

#steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
#date: The date on which the measurement was taken in YYYY-MM-DD format
#interval: Identifier for the 5-minute interval in which measurement was taken
library(ggplot2)
library(lubridate)


#Download & unzip dataset if it does not exist:
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
ls()
unzip(temp)

#read dataset Process/transform the data 
#(if necessary) into a format suitable for your analysis

activityRR<- read.csv("activity.csv")
summary(activityRR)
activityRR$date<-ymd(activityRR$date)
summary(activityRR)
head(activityRR)

#Histogram of the total number of steps taken each day
stepseachDay <- tapply(activityRR$steps, activityRR$date, sum, na.rm=TRUE)

qplot(stepseachDay,
      geom="histogram",
      main = "Histogram of the total number of steps taken each day", 
      xlab = "Steps",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))


hist(stepseachDay,col = "blue", main="Histogram of the total number of steps taken each day", xlab= "steps each day")

#Mean and median number of steps taken each day
mean<-mean(stepseachDay)
median(stepseachDay)

#Time series plot of the average number of steps taken had to make a new matrix as the old one was not labled columns
totalstepsperday <- aggregate(steps ~ date, data = activityRR, FUN = sum, na.rm = TRUE)
head(totalstepsperday)


ggplot(totalstepsperday, aes(date, steps)) + geom_line() +
  xlab("") + ylab("Daily Steps")

#The 5-minute interval that, on average, contains the maximum number of steps
ggplot(activityRR, aes(x = interval , y = steps))+
  geom_line(color="blue", size=1) + 
  labs(title = "steps per 5 minute intervals", x = "5 min Interval", y = " Steps")

summary(activityRR)

max_step<- max(activityRR$steps, na.rm=T)
                      
activityRR[max_step, ]$interval


#replace missing values

activityRR$steps[is.na(activityRR$steps)]<-mean(activityRR$steps, na.rm=TRUE)

stepseachDay <- tapply(activityRR$steps, activityRR$date, sum, na.rm=TRUE)

qplot(stepseachDay,
      geom="histogram",
      main = "Histogram of the total number of steps taken each day", 
      xlab = "Steps",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))


hist(stepseachDay,col = "blue", main="Histogram of the total number of steps taken each day", xlab= "steps each day")


#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
