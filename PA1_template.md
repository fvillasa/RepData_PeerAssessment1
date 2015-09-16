# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv(unz("activity.zip", "activity.csv"), stringsAsFactors=FALSE, na.strings = "NA")
activity$date <- as.Date(activity$date, format= "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
hist(aggregate(steps ~ date, data = activity, sum)$steps, xlab = "Daily steps", main = "Histogram of Daily Steps")
```

![](PA1_template_files/figure-html/mean and median-1.png) 

```r
options(scipen=2)
StepsMean <- mean(aggregate(steps ~ date, data = activity, sum)$steps)
StepsMedian <- median(aggregate(steps ~ date, data = activity, sum)$steps)
```
The mean of the total number of steps taken by day is 10766.1886792.

The median of the total number of steps taken by day is 10765.

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
