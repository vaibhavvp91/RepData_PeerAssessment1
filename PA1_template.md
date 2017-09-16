# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
df <- read.csv(unz("activity.zip", "activity.csv"), header=T, sep=",")
head(df)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?

1.  Calculate the total number of steps taken per day


```r
df_withhout_NA <- na.omit(df)

steps_datewise <- aggregate(steps~date, df_withhout_NA, sum)
print(steps_datewise)
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

2.  Make a histogram of the total number of steps taken each day


```r
hist(steps_datewise$steps,
     main = paste("Histogram of the total number of steps taken each day"),
     xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3.  Calculate and report the mean and median of the total number of steps taken per day


```r
mean(steps_datewise$steps)
```

```
## [1] 10766.19
```

```r
median(steps_datewise$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_interwalwise <- aggregate(steps~interval, df_withhout_NA, mean)
plot(steps_interwalwise$interval, steps_interwalwise$steps, type = "l",
     main = paste("Time series plot of the average number of steps taken"),
     xlab = "Interval",
     ylab = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
steps_interwalwise[steps_interwalwise$steps == max(steps_interwalwise$steps),]$interval
```

```
## [1] 835
```


## Imputing missing values

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(df$steps))
```

```
## [1] 2304
```

2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.
4.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# Filling NA values with mean of same interval
df_filled_NA <- df

for (interval in steps_interwalwise$interval){
    
    mean_steps <- mean(df_filled_NA[df_filled_NA$interval == interval,]$steps, na.rm = TRUE)
    
    df_filled_NA[df_filled_NA$interval == interval,]$steps[which(is.na(df_filled_NA[df_filled_NA$interval == interval,]$steps))] <- mean_steps
    
}

steps_datewise_new <- aggregate(steps~date, df_filled_NA, sum)

hist(steps_datewise_new$steps,
     main = paste("Histogram of the total number of steps taken each day (Modified Data)"),
     xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
mean(steps_datewise_new$steps)
```

```
## [1] 10766.19
```

```r
median(steps_datewise_new$steps)
```

```
## [1] 10766.19
```

Impact : Mean stays the same, but median changes

## Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
df_filled_NA$date <- as.Date(df_filled_NA$date)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

df_filled_NA$wDay <- factor((weekdays(df_filled_NA$date) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

fivemin2<- aggregate(steps ~ interval + wDay, data = df_filled_NA, FUN = mean, na.rm = TRUE)
head(fivemin2)
```

```
##   interval    wDay       steps
## 1        0 weekend 0.214622642
## 2        5 weekend 0.042452830
## 3       10 weekend 0.016509434
## 4       15 weekend 0.018867925
## 5       20 weekend 0.009433962
## 6       25 weekend 3.511792453
```

```r
library(ggplot2) 

ggplot(fivemin2, aes(x =interval , y=steps, color=wDay)) +
    geom_line() +
    labs(title = "Ave Daily Steps (type of day)", x = "Interval", y = "Total Number of Steps") +
    facet_wrap(~ wDay, ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


