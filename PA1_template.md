---
title: "Reproducible Research Project 1"
output: 
  html_document: 
    keep_md: yes
---



1. Load and format data correctly


```r
activity <- read.csv("activity.csv")
activity$date <- as.POSIXct(activity$date, tz="", "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)
```

2. Histogram of total number of steps taken each day?


```r
        tot_step <- with(activity, aggregate(steps, by = list(date),FUN=sum, na.rm =TRUE))
        names(tot_step) <- c("date", "steps")
        hist(tot_step$steps, main="Total Number of Steps Per Day", xlab = "Total Per Day", breaks = seq(0,25000, by=500), col="red")
```

![](PA1_template_files/figure-html/total_steps-1.png)<!-- -->

3. Mean of total number of steps taken per day

```r
      mean_stpes <-  mean(tot_step$steps)
        mean_stpes
```

```
## [1] 9354
```

3. Median of total number fo steps per day

```r
        median_step <- median(tot_step$steps)
        median_step
```

```
## [1] 10395
```

4 Time series plot of average number of steps taken

```r
        avgdy_act  <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
        names(avgdy_act) <- c ("interval", "average")
        plot(avgdy_act$interval, avgdy_act$average, type = "l", col="red", ylab ="Average Number Steps", xlab = "Interval", main = "Time series plot of the average number of steps taken")
```

![](PA1_template_files/figure-html/tmser_avg_num-1.png)<!-- -->

5. The 5-minute interval that, on average, contains the maximum number of steps

```r
        avgdy_act[which.max(avgdy_act$average), ]$interval
```

```
## [1] 835
```

6. Code to describe and show a strategy for imputing missing data
        Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
        

```r
        sum(is.na(activity$steps))
```

```
## [1] 2304
```
        Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
        

```r
        null_steps <- avgdy_act$average[match(activity$interval, avgdy_act$interval)]
```
        Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
        activity_null <- transform(activity, steps = ifelse(is.na(activity$steps), yes = null_steps, no = activity$steps))
        total_steps_null <- aggregate(steps ~ date, activity_null, sum)
        names(total_steps_null) <- c("date", "daily_steps")
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
        hist(total_steps_null$daily_steps, main="Total Number of Steps Per Day", xlab = "Total Per Day", breaks = seq(0,25000, by=500), col="red")
```

![](PA1_template_files/figure-html/hist_null_rmv-1.png)<!-- -->

The mean is now

```r
        new_mean <- mean(total_steps_null$daily_steps)
        new_mean
```

```
## [1] 10766
```
The median is now

```r
        new_median <- median(total_steps_null$daily_steps)
        new_median
```

```
## [1] 10766
```
Comparing mean and median with and without nulls
        Before Null removal mean was 9354.2295 and after it is 10766.1887.
        Before Null removal median was 10395 and after it is 10766.1887.

Are there differences in activity patterns between weekdays and weekends?

Add Weekday, Weekend lable

```r
        activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
        activity$datetype <- sapply(activity$date, function(x) {
        if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })       
```
And Plot


```r
        library(ggplot2)
        activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
        plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
        print(plot)
```

![](PA1_template_files/figure-html/plot_wkdayVswkend-1.png)<!-- -->





        
