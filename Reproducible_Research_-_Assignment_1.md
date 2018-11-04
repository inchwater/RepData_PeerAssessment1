Task 1 - Load the data & plot the histogram of the total number of steps taken each day
---------------------------------------------------------------------------------------

    setwd("C:/Users/Siddharth/Documents/Careers/Siddharth/DataScienceSpecialization/Reproducible Research/Week 2/")

    activity <- read.csv("activity.csv")
    activity <- na.omit(activity)

    total_steps <- tapply(activity$steps, activity$date, sum)

    total_steps <- total_steps[!is.na(total_steps)]

    hist(total_steps, col = "red", xlab = "Total number of steps taken each day", main = "Histogram of steps taken each day")

![](Reproducible_Research_-_Assignment_1_files/figure-markdown_strict/load%20data-1.png)

    mean_total_steps <- as.integer(mean (total_steps))

    median_total_steps <- median (total_steps)

The mean for the number of steps taken each day is 10766

The median for the number of steps taken each day is 10765

Task 2 - Find the average daily pattern
---------------------------------------

Calculate the average number of steps taken for each time interval
across all the days

    mean_steps <- tapply(activity$steps, activity$interval, mean)

    mean_steps_df <- data.frame(mean_steps)

    mean_steps_df2 <- cbind(mean_steps_df, activity$interval[1:288])

    plot(mean_steps_df2$`activity$interval[1:288]`, mean_steps_df2$mean_steps, type = "l", xlab = "Time interval", ylab = "Average steps taken per time interval across all days", main = "Average daily pattern")

![](Reproducible_Research_-_Assignment_1_files/figure-markdown_strict/average%20daily%20pattern-1.png)

    max_steps <- as.integer(max(mean_steps))

The maximum number of steps on average occurs at the 835th time interval
and it is 206

Task 3 - Imputing missing values
--------------------------------

Calculate the total number of missing values (NAs)

    activity <- read.csv("activity.csv")

    activity_na_count <- activity[rowSums(is.na(activity)),]

    nos_na <- nrow(activity_na_count)

The total number of missing values in the dataset is 2304

Apply a strategy to replace missing values - replace missing values with
the mean for that time interval

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    mean_steps_df2 <-  mean_steps_df2 %>% rename(interval = `activity$interval[1:288]`)

    activity <- read.csv("activity.csv")

    for (i in 1: 17568){
            if (is.na(activity$steps[i] == TRUE)){
                    interval <- activity$interval[i]
                    activity$steps[i] <- mean_steps_df2$mean_steps[match(interval, mean_steps_df2$interval)]
            }
            i <- i + 1
            
    }

    total_steps_nona <- tapply(activity$steps, activity$date, sum)

    total_steps_nona <- total_steps_nona[!is.na(total_steps)]

    hist(total_steps_nona, col = "red", xlab = "Total number of steps taken each day", main = "Histogram of steps taken each day")

![](Reproducible_Research_-_Assignment_1_files/figure-markdown_strict/replace%20missing%20values-1.png)

    mean_total_steps_nona <- as.integer(mean (total_steps_nona))

    median_total_steps_nona <- as.integer(median (total_steps_nona))

The mean for the number of steps taken each day with NAs removed is
10766

The median for the number of steps taken each day with NAs removed is
10766

These values **DO NOT** differ from the estimates of the mean and median
in the first part

    par(mfrow = c(1,2))

    hist(total_steps, col = "red", xlab = "Total number of steps taken each day", main = "No NAs")

    hist(total_steps_nona, col = "blue", xlab = "Total number of steps taken each day", main = "Imputing NAs")

![](Reproducible_Research_-_Assignment_1_files/figure-markdown_strict/compare%20total%20steps-1.png)

It can be seen from the graphs above that imputing NAs bumps up the
frequency for the number of steps in the 10,000-15,000 range but the
overall pattern remains the same

Task 4 - Differences in activity patterns between weekdays and weekends
-----------------------------------------------------------------------

    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    activity$date <-  ymd(activity$date)

    activity$day <- weekdays(activity$date, abbreviate = TRUE)

    activity$day_type <- ifelse(weekdays(activity$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

    activity_wekd <- activity %>% filter(day_type == "weekday")

    activity_wekend <- activity %>% filter(day_type == "weekend")

    par(mfrow = c(2,1))

    plot(activity_wekd$interval,activity_wekd$steps, type = "l", xlab = " Interval", ylab = "Number of steps", main = "Weekday")

    plot(activity_wekend$interval, activity_wekend$steps, type = "l", xlab = "Interval", ylab = "Number of steps", main = "Weekend")

![](Reproducible_Research_-_Assignment_1_files/figure-markdown_strict/activity%20weekdays%20or%20weekends-1.png)
