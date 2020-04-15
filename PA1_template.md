---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
setwd("C:/Users/acer/ProgrammingAssignment2/data/RepData_PeerAssessment1")
rawData <- read.csv("activity.csv")

## What is mean total number of steps taken per day?
sum_data <- aggregate(rawData$steps, by=list(rawData$date), FUN=sum, na.rm=TRUE)
names(sum_data) <- c("date", "total")
hist(sum_data$total, 
     main="Total Step Taken Each Day",
     breaks=seq(from=0, to=25000, by=2500),
     xlab="Total Steps", 
     border="blue", 
     col="green",
     ylim=c(0,40))
     mean(sum_data$total)
     median(sum_data$total)


## What is the average daily activity pattern?
meanData <- aggregate(rawData$steps, by=list(rawData$interval), FUN=mean, na.rm=TRUE)

# Rename the attributes
names(meanData) <- c("interval", "mean")

plot(meanData$interval, 
     meanData$mean,
     type = "l", 
     col="green", 
     lwd = 2,
     xlab="Intervel [minutes]", 
     ylab="Average num of Steps", 
     main="Time-series of the average number of steps per intervals")
     #We find the position of the maximum mean
maxPosition <- which(meanData$mean == max(meanData$mean))
# We lookup the value of interval at this position
maxInterval <- meanData[maxPosition, 1]
maxInterval


## Imputing missing values
NA_count <- sum(is.na(rawData$steps))
# Find the NA positions
NApos <- which(is.na(rawData$steps))
head(NApos)
# Create a vector of means
meanVec <- rep(mean(rawData$steps, na.rm=TRUE), times=length(NApos))
head(meanVec)
rawData[NApos, "steps"] <- meanVec #replace NA with mean value
# Calculate and report the mean and median total number of steps taken per day
sumData <- aggregate(rawData$steps, by=list(rawData$date), FUN=sum)
names(sumData) <- c("date", "total") #rename x and y

hist(sumData$total,
     breaks=seq(from=0, to=25000, by=2500),
     xlab="Total Steps", 
     border="blue", 
     col="green",
     ylim=c(0,40),
     main="Total number of steps taken each day")
     mean(sumData$total)
     median(sumData$total)


## Are there differences in activity patterns between weekdays and weekends?
#  indicating whether a given date is a weekday or weekend day.

weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

rawData$date <- as.Date(rawData$date)
rawData$day <- sapply(rawData$date, FUN=weekday.or.weekend)
library(ggplot2)
averages <- aggregate(steps ~ interval + day, data=rawData, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("Interval") + ylab("Number of steps") 
