Loading and preprocessing the Activityset
-----------------------------------------

Show any code that is needed to:

1.Load the Activity (i.e. read.csv()) 2.Process/transform the Activity
(if necessary) into a format suitable for your analysis

    library(knitr)

    ## Warning: package 'knitr' was built under R version 3.3.3

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)

    active <- read.csv('activity.csv')

    # remove NA from Activityset
    Activity <- active[ with (active, { !(is.na(steps)) } ), ]

    # print out first 6 rows
    head(Activity)

    ##     steps       date interval
    ## 289     0 2012-10-02        0
    ## 290     0 2012-10-02        5
    ## 291     0 2012-10-02       10
    ## 292     0 2012-10-02       15
    ## 293     0 2012-10-02       20
    ## 294     0 2012-10-02       25

    by_day <- group_by(Activity, date)
    steps_by_day <- summarise(by_day, total = sum(steps))
    steps_by_day

    ## # A tibble: 53 × 2
    ##          date total
    ##        <fctr> <int>
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
    ## # ... with 43 more rows

    summary(steps_by_day)

    ##          date        total      
    ##  2012-10-02: 1   Min.   :   41  
    ##  2012-10-03: 1   1st Qu.: 8841  
    ##  2012-10-04: 1   Median :10765  
    ##  2012-10-05: 1   Mean   :10766  
    ##  2012-10-06: 1   3rd Qu.:13294  
    ##  2012-10-07: 1   Max.   :21194  
    ##  (Other)   :47

    hist(steps_by_day$total, main="Histogram of total number of steps per day", 
         xlab="Total number of steps in a day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    summary(steps_by_day)

    ##          date        total      
    ##  2012-10-02: 1   Min.   :   41  
    ##  2012-10-03: 1   1st Qu.: 8841  
    ##  2012-10-04: 1   Median :10765  
    ##  2012-10-05: 1   Mean   :10766  
    ##  2012-10-06: 1   3rd Qu.:13294  
    ##  2012-10-07: 1   Max.   :21194  
    ##  (Other)   :47

What is the average daily activity pattern?
-------------------------------------------

1.Make a time series plot (i.e. type = "l") of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days
(y-axis)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    # preprocessing data for plot
    steps_by_interval <- aggregate(steps ~ interval, Activity, mean)

    # create a time series plot 
    plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
         main="Average number of steps over all days", xlab="Interval", 
         ylab="Average number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    # find row with max of steps
    max_steps_row <- which.max(steps_by_interval$steps)

    # find interval with this max
    steps_by_interval[max_steps_row, ]

    ##     interval    steps
    ## 104      835 206.1698

The interval 835 has the maximum average value of steps (206.1698).

Imputing missing values
-----------------------

1.Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

2.Devise a strategy for filling in all of the missing values in the
dataset. The strategy does not need to be sophisticated. For example,
you could use the mean/median for that day, or the mean for that
5-minute interval, etc.

3.Create a new dataset that is equal to the original dataset but with
the missing data filled in.

4.Make a histogram of the total number of steps taken each day and
Calculate and report the mean and median total number of steps taken per
day. Do these values differ from the estimates from the first part of
the assignment? What is the impact of imputing missing data on the
estimates of the total daily number of steps?

    sum(is.na(active))

    ## [1] 2304

**Total number of rows with NA's is 2304.**

I picked the strategy of replacing NA's with the mean for that 5-minute
interval.

    data_imputed <- active
    for (i in 1:nrow(data_imputed)) {
      if (is.na(data_imputed$steps[i])) {
        interval_value <- data_imputed$interval[i]
        steps_value <- steps_by_interval[
          steps_by_interval$interval == interval_value,]
        data_imputed$steps[i] <- steps_value$steps
      }
    }

    # calculate  total number of steps taken each day
    df_imputed_steps_by_day <- aggregate(steps ~ date, data_imputed, sum)
    head(df_imputed_steps_by_day)

    ##         date    steps
    ## 1 2012-10-01 10766.19
    ## 2 2012-10-02   126.00
    ## 3 2012-10-03 11352.00
    ## 4 2012-10-04 12116.00
    ## 5 2012-10-05 13294.00
    ## 6 2012-10-06 15420.00

    hist(df_imputed_steps_by_day$steps, main="Histogram of total number of steps per day (imputed)", 
         xlab="Total number of steps in a day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    # mean and median
    mean(df_imputed_steps_by_day$steps)

    ## [1] 10766.19

    median(df_imputed_steps_by_day$steps)

    ## [1] 10766.19

    # mean and median of data without NA's
    mean(steps_by_day$total)

    ## [1] 10766.19

    median(steps_by_day$total)

    ## [1] 10765

**Mean values stays the same but therer is slight difference in meadian
value.**

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

For this part the weekdays() function may be of some help here. Use the
dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels -
    "weekday" and "weekend" indicating whether a given date is a weekday
    or weekend day.

2.  Make a panel plot containing a time series plot (i.e. type = "l") of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

<!-- -->

    data_imputed['type_of_day'] <- weekdays(as.Date(data_imputed$date))
    data_imputed$type_of_day[data_imputed$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
    data_imputed$type_of_day[data_imputed$type_of_day != "weekend"] <- "weekday"
    # convert type_of_day from character to factor
    data_imputed$type_of_day <- as.factor(data_imputed$type_of_day)

    # calculate average steps by interval across all days
    df_imputed_steps_by_interval <- aggregate(steps ~ interval + type_of_day, data_imputed, mean)

    # creat a plot
    qplot(interval, 
          steps, 
          data = df_imputed_steps_by_interval, 
          type = 'l', 
          geom=c("line"),
          xlab = "Interval", 
          ylab = "Number of steps", 
          main = "") +
      facet_wrap(~ type_of_day, ncol = 1)

    ## Warning: Ignoring unknown parameters: type

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)
