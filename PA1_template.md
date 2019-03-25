---
title: "Activity monitorig data"
author: "OscarAMtz"
date: "20 de marzo de 2019"
output: html_document
---

[RepData_PeerAssessment1](http://htmlpreview.github.io/?https://github.com/oscaramtz/RepData_PeerAssessment1/blob/master/PA1_template.html)

```{R, echo=FALSE}
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "repdata data activity.zip")
unzip("./repdata data activity.zip", exdir = "./data")
```

## Cleaning data

```{R}
activities <- read.csv("./data/activity.csv")
str(activities)

activities$date <- as.POSIXlt.character(paste(activities$date, activities$interval)) 

activities <- read.csv("./data/activity.csv")
activities$date <- as.Date(activities$date, "%Y-%m-%d")


activities$time <- sprintf("%04d", activities$interval)

activities$time <- gsub('^([0-9]{2})', '\\1:', activities$time)

#activities$time <- factor(activities$time, ordered = T)
activities$time <- paste0(activities$time, ":00")

activities$completedate <- as.POSIXlt.character(paste(activities$date, activities$time))

activities$time <- as.POSIXct(activities$time, format = "%H:%M:%S")
CleanDataActivities <- activities

```
## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day
2. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day

```{R}

## Summary table, sum of steps by date
dailySteps <- aggregate(steps~as.Date(completedate),
                        CleanDataActivities,
                        sum)

## Plotting histogram "Total number of steps taken each day"
hist(dailySteps$steps,
     xlab = "Frequency for daily",
     main = "Total number of steps taken each day")
abline(v=mean(dailySteps$steps), col = "darkblue")
text(c(22000,21690),
     c(25,23), 
     c(sprintf("Mean = %.3f", mean(dailySteps[, 2])),
       sprintf("Median = %.0f", median(dailySteps[, 2]))))
```
![Plot 1](https://github.com/oscaramtz/RepData_PeerAssessment1/blob/master/instructions_fig/Plot%201.png)


## What is the average daily activity pattern?

1. Make a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{R}
## summary table, Average daily activity pattern
meanIntervalSteps <- aggregate(steps~time,
                               CleanDataActivities,
                               mean)

meanIntervalSteps$time <- as.POSIXct(meanIntervalSteps$time, format = "%H:%M:%S")

## Maximum mean
maxmean <- which.max(meanIntervalSteps$steps)

## plot "Average daily activity pattern"
library(ggplot2)
ggplot(meanIntervalSteps, aes(x = time, y = steps)) +
  geom_line(col = "darkblue") +
  geom_point(alpha = 0.075) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hour") +
  labs(title = "Average daily activity pattern") +
  xlab("Hours") +
  ylab("Steps average") +
  geom_text(aes(x = meanIntervalSteps$time[maxmean],y = meanIntervalSteps$steps[maxmean], label = paste(format(meanIntervalSteps$time[maxmean], "%H:%M"), "\n", round(meanIntervalSteps$steps[maxmean],2))), hjust = 0, size = 4) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
```

![Plot 2](https://github.com/oscaramtz/RepData_PeerAssessment1/blob/master/instructions_fig/Plot%202.png)

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
2. The strategy for filling in all of the missing values in the dataset was the mean for that 5-minute interval.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{R}
## Mapping the missing values
missingStepValues <- which(is.na(CleanDataActivities$steps))

## n of missings
length(missingStepValues)

## making comparable vars
meanIntervalSteps$timehms <- format(meanIntervalSteps$time, "%H:%M:%S")
CleanDataActivities$time <- format(CleanDataActivities$completedate, "%H:%M:%S")

## duplicate steps var
CleanDataActivities$fixedsteps <- CleanDataActivities$steps

## loop for NA change
for (i in missingStepValues){
  CleanDataActivities[i, "fixedsteps"] <- meanIntervalSteps[CleanDataActivities[i, "time"] == meanIntervalSteps$timehms, "steps"]
}

## summary table, Total number of steps taken each day with inputed NA's
NAImputeddailySteps <- aggregate(fixedsteps~as.Date(completedate),
                        CleanDataActivities,
                        sum)

## Plotting histogram "Total number of steps taken each day, inputed NA"
hist(NAImputeddailySteps$fixedsteps,
     xlab = "Frequency for daily",
     main = "Total number of steps taken each day, inputed NA")
abline(v=mean(NAImputeddailySteps$fixedsteps), col = "darkblue")
text(c(23000,22600),c(30,28), c(
  sprintf("Mean = %.3f", mean(NAImputeddailySteps[, 2])),
  sprintf("Median = %.0f", median(NAImputeddailySteps[, 2]))))
```
![Plot 3](https://github.com/oscaramtz/RepData_PeerAssessment1/blob/master/instructions_fig/Plot%203.png)


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```{R}
# Weekday clissified 
CleanDataActivities$days <- weekdays(CleanDataActivities$completedate)

weekend <- unique(CleanDataActivities$days)[6:7]

## var weekday or weekend
CleanDataActivities$weekday <- ifelse(CleanDataActivities$days 
                                      %in% weekend, 
                                      "Weekend", 
                                      "Weekday")


## weekdays Dataframe
weekdaySteps <- CleanDataActivities[CleanDataActivities$weekday == "Weekday",]

weekdayMeanSteps <- aggregate(fixedsteps~interval,
                        weekdaySteps,
                        mean)

weekdayMeanSteps$weekday <- "weekday"


## weekend Dataframe
weekendSteps <- CleanDataActivities[CleanDataActivities$weekday == "Weekend",]

weekendMeanSteps <- aggregate(fixedsteps~interval,
                        weekendSteps,
                        mean)

weekendMeanSteps$weekday <- "weekend"


## row bind
daysMeansteps <- rbind(weekendMeanSteps,weekdayMeanSteps)


# Plot
library(lattice)
xyplot(fixedsteps~interval|weekday, 
       data = daysMeansteps,
       type="l", 
       ylab="Number of steps", 
       xlab="Interval", 
       layout=c(1, 2)
       )

```
![Plot 4](https://github.com/oscaramtz/RepData_PeerAssessment1/blob/master/instructions_fig/Plot%204.png)
