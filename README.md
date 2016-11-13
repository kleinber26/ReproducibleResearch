# ReproducibleResearch

---
title: "Reproducible Research Course Project 1"
output: html_document
---

###Loading the data
```{r, echo=TRUE}
if (!file.exists("activity.zip")) {
fileURL <<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, "activity.zip", method = "curl")
}
if(!file.exists("activity")) {
unzip("activity.zip")
}

activity <<- read.csv("activity.csv", header = TRUE)
str(activity)
```
###What is the mean total number of steps taken per day? 
1. Calculate the total number of steps taken per day.
```{r, echo=TRUE}
activitybyday <- aggregate(activity$steps, list(date=activity$date), sum)
colnames(activitybyday) <- c("date", "steps")
head(activitybyday)
```
2. Make a histogram of the total number of steps taken each day. 
```{r, echo=TRUE}
hist(activitybyday$steps, main = "Total Number of Steps Taken Each Day", xlab = "Steps", breaks=10)
```




3. Calculate the mean and median of the total number of steps taken per day. 
```{r, echo=TRUE}
mean(activitybyday$steps, na.rm=TRUE)
median(activitybyday$steps, na.rm=TRUE)
```
###What is the average daily activity pattern? 
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
```{r}
activitybyinterval <- aggregate(activity$steps, list(interval=activity$interval), mean, na.rm=TRUE)
colnames(activitybyinterval) <- c("interval", "steps")
plot(activitybyinterval$interval, activitybyinterval$steps, type = "l", xlab = "5-min interval", ylab = "Average number of steps taken")
```




2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 
```{r, echo=TRUE}
activitybyinterval[activitybyinterval$steps==max(activitybyinterval$steps), ]
```
Interval 835 contains the maximum number of steps.   

###Imputing missing values
1. Calculate the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r, echo=TRUE}
nrow(activity[!complete.cases(activity),])
```
2. Devise a strategy for filling in all of the missing values in the dataset.  
For the missing values in the dataset, I used the mean for that 5-minute interval.  
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r, echo=TRUE}
meansteps <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
activity2 <- activity
activity2$steps[is.na(activity$steps)] <- meansteps
head(activity2)
```
4. Make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  
```{r, echo=TRUE}
activitybyday2 <- aggregate(activity2$steps, list(date=activity2$date), sum)
colnames(activitybyday2) <- c("date", "steps")
head(activitybyday2)
hist(activitybyday2$steps, main = "Total Number of Steps Taken Each Day", xlab = "Steps", breaks=10)
mean(activitybyday2$steps)
median(activitybyday2$steps)
```
Inputting missing data on the estimates of the total daily number of steps does not affect the mean (the mean total number of steps taken per day with missing values ignored and filled in is 10766.19); however, the median is affected (the median with missing values ignored is 10765 and the median with missing values filled in is 10766.19).  

###Are there differences in activity patterns between weekdays and weekends? 
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. 
```{r, echo=TRUE}
activity3 <- activity2
activity3$date <- as.Date(activity3$date)
activity3 <- transform(activity3, Dayofweek = weekdays(activity3$date))
activity3$Dayofweek <- ifelse(activity3$Dayofweek == "Saturday" | activity3$Dayofweek == "Sunday", c("weekend"), c("weekday"))
activity3$Dayofweek <- as.factor(activity3$Dayofweek)
head(activity3)
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5 minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r, echo=TRUE}
activitybyinterval2 <- aggregate(activity3$steps, list(interval=activity3$interval, dayofweek=activity3$Dayofweek), mean)
colnames(activitybyinterval2) <- c("interval", "dayofweek", "steps")
library(lattice)
xyplot(steps ~ interval|dayofweek, data=activitybyinterval2, type="l", layout= c(1,2))
```