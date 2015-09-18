---
title: "Reproducible Research_PA1"
author: "Paymon Hashemi"
date: "17 Sep 2015"
output: html_document
---

##Loading and preprocessing the data
1. Load the data
```{r echo=TRUE}
ActivityData <- read.csv("C:/Users/PAYMON/RepData_PeerAssessment1/activity/activity.csv")
```

2. Process/transform data into a format suitable for my analysis
```{r echo=TRUE}
ActivityData$date <- as.Date(ActivityData$date, format = "%Y-%m-%d")
ActivityData$interval <- as.factor(ActivityData$interval)
```

3. Explore the data using str() and head ()
```{r echo=TRUE}
str(ActivityData)
head(ActivityData)
```

##What is mean total number of steps taken per day?

0. Use tapply to develop a vector prior to developing a histogram plot
```{r echo=TRUE}
StepsEachDay <- tapply(ActivityData$steps, ActivityData$date, sum, na.rm = TRUE)
```

1. Make a histogram of the total number of steps taken each day
```{r echo=TRUE}
hist(StepsEachDay, col="blue", 
     xlab = 'Total steps each day', ylab= 'Frequency', 
     main = "Total # of steps taken each day")
```

2. Calculate and report the mean and median total number of steps taken per day
```{r echo=TRUE}
StepsMean <- mean(StepsEachDay, na.rm=TRUE)
StepsMean
StepsMedian <- median(StepsEachDay, na.rm=TRUE)
StepsMedian
```

##What is the average daily activity pattern?

1. Make a time series plot
```{r echo=TRUE}
TimeSeries <- tapply(ActivityData$steps, ActivityData$interval, mean, na.rm = TRUE)
plot(row.names(TimeSeries), TimeSeries, type = "l", 
     xlab = "5-minute interval", ylab = "Average across days", 
     main = "Average # of steps taken", col = "green")
```

2. Determine which 5-minute interval contains the maximum number of steps
```{r echo=TRUE}
MaxInterval <- which.max(TimeSeries)
names(MaxInterval)
```

##Imputing missing values

1. Calculate and report the total number of missing values in the dataset
```{r echo=TRUE}
ActivityData_NA <- sum(is.na(ActivityData))
ActivityData_NA
```

2. Devise a strategy for filling in all of the missing values in the dataset
3. Create a new dataset that is equal to the original, but with missing data filled in
(Note: This is a combined step)
```{r echo=TRUE}
library(Hmisc)
ActivityData_Imputed <- ActivityData
ActivityData_Imputed$steps <- impute(ActivityData$steps, fun=mean)
```

4. Make a histogram of the total number of steps taken each day
```{r echo=TRUE}
StepsEachDay_Imputed <- tapply(ActivityData_Imputed$steps, ActivityData_Imputed$date, sum)
hist(StepsEachDay_Imputed, 
     xlab = 'Total steps each day (Imputed)', ylab= 'Frequency', 
     main = 'Steps Each Day (Imputed)')
```

5. Calculate and report the mean and median total number of steps taken per day
```{r echo=TRUE}
StepsMean_Imputed <- mean(StepsEachDay_Imputed, na.rm=TRUE)
StepsMean_Imputed
StepsMedian_Imputed <- median(StepsEachDay_Imputed, na.rm=TRUE)
StepsMedian_Imputed
```

##Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with "Weekday" and "weekend" levels
```{r echo=TRUE}
Day <- weekdays(ActivityData_Imputed$date)
DayType <- vector()
for (i in 1:nrow(ActivityData_Imputed)) {
        if (Day[i] == "Saturday") {
                DayType[i] <- "Weekend"
        } else if (Day[i] == "Sunday") {
                DayType[i] <- "Weekend"
        } else {
                DayType[i] <- "Weekday"
        }
}

ActivityData_Imputed$DayType <- DayType
ActivityData_Imputed$DayType <- factor(ActivityData_Imputed$DayType)

StepsDay <- aggregate(steps ~ interval + DayType, data = ActivityData_Imputed, mean)
names(StepsDay) <- c("interval", "DayType", "steps")
```

2. Make a panel plot containing a time series plot
```{r echo=TRUE}
Imputed_TimeSeries <- aggregate(steps ~ interval + DayType, data=ActivityData_Imputed, mean)
ggplot(Imputed_TimeSeries, aes(x=interval, y=steps)) + 
        geom_line() + 
        facet_wrap(~ DayType, nrow=2, ncol=1) +
        xlab("5-minute interval") + 
        ylab("Average # of steps")
```
