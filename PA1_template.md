##Reproducible Research Project 1
echo=TRUE
###Loading and preprocessing the data
file <- read.csv('activity.csv')
###What is mean total number of steps taken per day?
day_stps <- aggregate(steps ~ date, file, sum)
hist(day_stps$steps, main = paste("Total number of steps taken per day"), col="red", xlab="Number of steps")
day_mean <- mean(day_stps$steps)
print (day_mean)
day_median <- median(day_stps$steps)
print (day_median)
###What is the average daily activity pattern?
interval_steps <- aggregate(steps ~ interval, file, mean)
plot(interval_steps$interval,interval_steps$steps, type="l", xlab="Interval", ylab="Number of steps",main="Average number of steps per day by interval")
interval_max <- interval_steps[which.max(interval_steps$steps),1]
print (interval_max)
###Imputing missing values
missing <- sum(is.na(file))
print (missing)
steps <- data.frame(file$steps)
steps[is.na(steps),] <- ceiling(tapply(X=file$steps,INDEX=file$interval,FUN=mean,na.rm=TRUE))
newfile <- cbind(steps, file[,2:3])
colnames(newfile) <- c("Steps", "Date", "Interval")
new_steps_sum <- aggregate(newfile$Steps, list(newfile$Date), sum)
colnames(new_steps_sum) <- c("Date", "Steps")
with(new_steps_sum, {barplot(height=Steps,main="Total steps taken per day",xlab="Dates",ylab="Steps per day",names.arg=Date,space=c(0))})
day_mean_new <- mean(new_steps_sum$Steps)
print (day_mean_new)
day_median_new <- median(new_steps_sum$Steps)
print (day_median_new)
means_sub <-day_mean_new - day_mean
print (means_sub)
median_sub <- day_median_new - day_median
print (median_sub)
###Are there differences in activity patterns between weekdays and weekends?
week_days <- data.frame(sapply(X=newfile$Date, FUN=function(day) {
  if (weekdays(as.Date(day)) %in% c("monday", "tuesday", "wednesday", "thursday", "friday")) {
    day <- "weekday"
  }
  else {
    day <- "weekend"
  } 
}))
newfile_week <- cbind(newfile, week_days)
colnames(newfile_week) <- c("Steps", "Date", "Interval", "Week_day")
steps_by_day_type <- aggregate(data=newfile_week,Steps ~ Week_day + Interval,FUN=mean)
library("lattice")
xyplot(data=steps_by_day_type,Steps ~ Interval|Week_day, xlab="Interval",ylab="Number of steps",layout=c(1,2),type="l")