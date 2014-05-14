Reproducible Research Assessment 1
========================================================

### Loading and preprocessing the data
Load the data 

```r
activity <- read.csv("activity.csv")
```


Transformation of the data 

```r
activity$date <- strptime(as.character(activity$date), format = "%Y-%m-%d")
```



### What is mean total number of steps taken per day?
Histogram of the total number of steps taken each day

```r
sumStepsPerDay <- sapply(split(activity$steps, as.character(activity$date)), 
    sum)
hist(sumStepsPerDay)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


mean and median total number of steps taken per day

```r
mean(sumStepsPerDay, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(sumStepsPerDay, na.rm = TRUE)
```

```
## [1] 10765
```



### What is the average daily activity pattern?
Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
aveStepsPerInterval <- sapply(split(activity$steps, activity$interval), function(x) mean(x, 
    na.rm = TRUE))
plot(names(aveStepsPerInterval), aveStepsPerInterval, type = "l", xlab = "Interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
names(aveStepsPerInterval[aveStepsPerInterval == max(aveStepsPerInterval)])
```

```
## [1] "835"
```

### Imputing missing values
Total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
totalNARows <- is.na(activity[[1]])
table(totalNARows)[["TRUE"]]
```

```
## [1] 2304
```

Strategy for filling in all of the missing values in the dataset is to take average of steps per interval and apply to the missing value in any interval

```r
aveStepsPerInterval <- sapply(split(activity$steps, activity$interval), function(x) mean(x, 
    na.rm = TRUE))
```

New dataset that is equal to the original dataset but with the missing data filled in.

```r
for (i in 1:length(aveStepsPerInterval)) {
    if (is.na(activity$steps[[i]])) 
        activity$steps2[[i]] <- aveStepsPerInterval[[i]]
}
```

Histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
sumStepsPerDay <- sapply(split(activity$steps2, as.character(activity$date)), 
    sum)
hist(sumStepsPerDay)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
mean(sumStepsPerDay, na.rm = TRUE)
```

```
## [1] 662.9
```

```r
median(sumStepsPerDay, na.rm = TRUE)
```

```
## [1] 494.5
```



### Are there differences in activity patterns between weekdays and weekends?
A new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
days <- weekdays(activity$date)
for (i in 1:length(activity$date)) {
    if (days[[i]] == "Saturday" | days[[i]] == "Sunday") 
        activity$dayType[[i]] <- "Weekend" else activity$dayType[[i]] <- "Weekday"
}
activity$dayType <- factor(activity$dayType)
```

A panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
par(mfrow = c(1, 2))
lapply(split(activity, activity$dayType), function(x) {
    aveStepsPerInterval <- sapply(split(x$steps, activity$interval), function(y) mean(y, 
        na.rm = TRUE))
    plot(names(aveStepsPerInterval), aveStepsPerInterval, type = "l", main = x$dayType[1])
})
```

```
## Warning: data length is not a multiple of split variable
## Warning: data length is not a multiple of split variable
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

```
## $Weekday
## NULL
## 
## $Weekend
## NULL
```


