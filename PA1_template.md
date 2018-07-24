---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

The following packages are required to run the R analysis code.


```r
library(knitr)
library(dplyr)
opts_chunk$set(message = FALSE, warning = FALSE, results = FALSE)
```

## Loading and preprocessing the data

Set the workng directory too RepData_PeerAssesment1


```r
file <- read.csv("activity.csv", na.strings = "NA")
```

## What is mean total number of steps taken per day?

The Groupby function is used to sort the dataframe by dates and calculate the sum, mean and median for each day.The histogram plot is saved to the instruction_figure folder.


```r
Grouped_bydate <- file %>% group_by(date) %>% 
    summarize(Mean = mean(steps, na.rm = TRUE), 
              Median = median(steps, na.rm = TRUE),
              Sum = sum(steps, na.rm = TRUE))

jpeg(file = "instructions_fig/Histogram_MeanSteps.jpeg")
hist(Grouped_bydate$Sum, main = "Total Number of Steps", xlab = "Steps")
dev.off()
```

The Dataframe is printed using the Kable function to show the mean and median steps for each day


```r
kable(Grouped_bydate[, 1:3], caption = "Mean and Median of steps taken per day")
```



Table: Mean and Median of steps taken per day

date                Mean   Median
-----------  -----------  -------
2012-10-01           NaN       NA
2012-10-02     0.4375000        0
2012-10-03    39.4166667        0
2012-10-04    42.0694444        0
2012-10-05    46.1597222        0
2012-10-06    53.5416667        0
2012-10-07    38.2465278        0
2012-10-08           NaN       NA
2012-10-09    44.4826389        0
2012-10-10    34.3750000        0
2012-10-11    35.7777778        0
2012-10-12    60.3541667        0
2012-10-13    43.1458333        0
2012-10-14    52.4236111        0
2012-10-15    35.2048611        0
2012-10-16    52.3750000        0
2012-10-17    46.7083333        0
2012-10-18    34.9166667        0
2012-10-19    41.0729167        0
2012-10-20    36.0937500        0
2012-10-21    30.6284722        0
2012-10-22    46.7361111        0
2012-10-23    30.9652778        0
2012-10-24    29.0104167        0
2012-10-25     8.6527778        0
2012-10-26    23.5347222        0
2012-10-27    35.1354167        0
2012-10-28    39.7847222        0
2012-10-29    17.4236111        0
2012-10-30    34.0937500        0
2012-10-31    53.5208333        0
2012-11-01           NaN       NA
2012-11-02    36.8055556        0
2012-11-03    36.7048611        0
2012-11-04           NaN       NA
2012-11-05    36.2465278        0
2012-11-06    28.9375000        0
2012-11-07    44.7326389        0
2012-11-08    11.1770833        0
2012-11-09           NaN       NA
2012-11-10           NaN       NA
2012-11-11    43.7777778        0
2012-11-12    37.3784722        0
2012-11-13    25.4722222        0
2012-11-14           NaN       NA
2012-11-15     0.1423611        0
2012-11-16    18.8923611        0
2012-11-17    49.7881944        0
2012-11-18    52.4652778        0
2012-11-19    30.6979167        0
2012-11-20    15.5277778        0
2012-11-21    44.3993056        0
2012-11-22    70.9270833        0
2012-11-23    73.5902778        0
2012-11-24    50.2708333        0
2012-11-25    41.0902778        0
2012-11-26    38.7569444        0
2012-11-27    47.3819444        0
2012-11-28    35.3576389        0
2012-11-29    24.4687500        0
2012-11-30           NaN       NA

## What is the average daily activity pattern?

The dataframe is grouped by intervals and the average for each interval is calculated using the summarize function. The time series plot is saved in the folder.


```r
Grouped_by_interval <- file %>% group_by(interval) %>% 
    summarise("Average Steps" = mean(steps, na.rm = TRUE))

jpeg(file = "instructions_fig/Daily_pattern.jpeg")

with(Grouped_by_interval, 
     plot(Grouped_by_interval$interval,Grouped_by_interval$`Average Steps`, 
          type ="l",xlab = "Interval", ylab = "Average Steps"))

dev.off()
```


```r
kable(Grouped_by_interval[which.max(Grouped_by_interval$'Average Steps'),], 
      caption = "Interval with maximum number of steps")
```

## Imputing missing values

The Following code is used to imput the missing values


```r
n <- which(is.na(file$steps))

x <- length(n)

Updated_file <- file

for ( i in 1:x) {
    Updated_file$steps[[n[i]]] <-Grouped_by_interval[
        Grouped_by_interval$interval ==
            Updated_file$interval[[n[i]]], "Average Steps"][[1]]
}
```

## Are there differences in activity patterns between weekdays and weekends?

The following creates a new file with factor levels Weekday and Weekend. After the values are used to generate a dataframe grouped by interval and then by weekday/Weekend. Base Plotting system is used to generate to panels of plot. After imputting the missing values it can be seen that Weekend activity is higher at certain intervals.


```r
Updated_file$date <- as.Date(Updated_file$date, "%Y-%m-%d")

Updated_file <- mutate(Updated_file, 
                         Day = factor((weekdays(Updated_file$date) %in% 
                                             c("Monday", "Tuesday", "Wednessday"
                                               , "Thursday", "Friday")), 
                                      labels = c("Weekday", "Weekend")))

Grouped_by_day <- Updated_file %>% group_by(interval,Day) %>%
    summarize(Mean = mean(steps, na.rm = TRUE))

jpeg(file = "instructions_fig/Activity_pattern.jpeg")

par(mfcol = c(2,1))

with(Grouped_by_day, plot(interval, Mean, type ="n", xlab = "Interval", ylab="Steps",
                          main ="Differences in activity pattern on Weekday
                          "))
with(subset(Grouped_by_day, Day == "Weekday"), lines(interval, Mean, col = "blue"))

with(Grouped_by_day, plot(interval, Mean, type ="n", xlab = "Interval", ylab="Steps",
                          main ="Differences in activity pattern on Weekend
                          "))
with(subset(Grouped_by_day, Day == "Weekend"), lines(interval, Mean, col = "red"))

dev.off()
```

