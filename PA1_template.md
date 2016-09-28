# PA1_template.Rmd
Glenn J Melton  
9/26/2016  



## R Markdown

## Loading and preprocessing the data

```r
## load libraries being used

suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(dplyr)))

## read prior downloaded coursera supplied data
actDf <- read.csv("activity.csv")
actDf$date <- as.Date(as.character(actDf$date))
summary(actDf)
```

```
##      steps           date               interval   
##  Min.   :  0    Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0    1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0    Median :2012-10-31   Median :1178  
##  Mean   : 37    Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12    3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806    Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```

## What is mean total number of steps taken per day?


```r
dailyStepTtl <- aggregate(steps ~ date, actDf,  sum)
```
Histogram Plot of aggregated summation of steps per day

```r
hist(dailyStepTtl$steps, col = "green", 
	xlab = "Distribution of Total Steps Taken Daily",
	main = paste("Histogram of daily Steps from",
		 		as.character(min(actDf$date)), "through",
				as.character(max(actDf$date)))
)
```

![](PA1_template_files/figure-html/histogram-dailysteps-1.png)<!-- -->

```r
meanSteps <- mean(dailyStepTtl$steps, na.rm = TRUE)
medianSteps <- median(dailyStepTtl$steps, na.rm = TRUE)
```

Mean daily step totals(thou.) are: 10.766, and the daily median step totals(thou.) are: 10.765.

## What is the average daily activity pattern?

```r
## aggregate mean of steps by interval
meanDailySteps <- aggregate(steps ~ interval, actDf,  mean)

with(meanDailySteps,
	 plot(interval, steps, type = "l", col = "blue", lwd = 2,
		  xlab = "Aggregated 5 Minute Recording Intervals",
		  ylab = "Mean Number of Steps Over Period",
		  main = paste("Time Series Analysis of Daily Steps from",
							as.character(min(actDf$date)), "through",
							as.character(max(actDf$date)))
		 )
)
abline(v = meanDailySteps[which.max(meanDailySteps$steps), 1], col = "red", lwd = 2)
```

![](PA1_template_files/figure-html/timeseries-of-intervals-1.png)<!-- -->

Calculate the interval with the most average steps


```r
maxSteps <- which.max(meanDailySteps$steps)
maxInterval <- meanDailySteps[maxSteps, 1]
```
The interval with the most steps is: 835 as shown in red on the above Time Series plot.

## Imputing missing values


```r
origMissingSteps <- sum(is.na(actDf$steps))
origMissingSteps
```

```
## [1] 2304
```
There are 2304 observations with missing data in the "steps" column.

Looking at the data, there are 288 intervals per day which is more granular rather than days of the week to replace the missing data. The function "iStep" returns the mean of the steps for the interval passed.


```r
## create working dataset from original
imputeDf <- actDf
## function that returns mean for an interval
iSteps <- function(x) {
    mean(imputeDf[imputeDf$interval == x, ]$steps, na.rm = TRUE)
}

## impute missing values useing above iStep funcion as needed
for(i in 1:nrow(imputeDf)) {
    if(is.na(imputeDf[i, ]$steps)) {
    	imputeDf[i, ]$steps <- iSteps(imputeDf[i, ]$interval)
    }
}
```
Plot the data with missing NA values imputed


```r
dailyStepTtl <- aggregate(steps ~ date, imputeDf,  sum)

hist(dailyStepTtl$steps, col = "green", 
	xlab = "Distribution of Total Steps Taken Daily",
	main = paste("Histogram of daily Steps from",
		 as.character(min(imputeDf$date)), "through",
		 as.character(max(imputeDf$date))))
```

![](PA1_template_files/figure-html/histogram-missing-data-1.png)<!-- -->

Calulate mean and median steps after imputing missing values


```r
imputedMeanSteps <- mean(dailyStepTtl$steps)
imputedMedianSteps <- median(dailyStepTtl$steps)
meanSteps; imputedMeanSteps
```

```
## [1] 10766
```

```
## [1] 10766
```

```r
medianSteps; imputedMedianSteps
```

```
## [1] 10765
```

```
## [1] 10766
```
The imputed Mean and Median are: 1.077\times 10^{4} and 1.077\times 10^{4} respectfully.
Not surprisingly, the mean and median are the same after imputing the values.

## Are there differences in activity patterns between weekdays and weekends?


```r
## defive character vector with weekdays in it
weekDays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
## add a new column to the imputed Data Frame and load the "daytype"
imputeDf$daytype <- c('weekend', 'weekday')[(weekdays(imputeDf$date, abbreviate=FALSE) %in% weekDays)+1L]
```
Create panel plot comparing weekday to weekend step activity levels

```r
suppressWarnings(suppressMessages(library(lattice)))
## aggregate data and plot with lattice
dailySteps <- aggregate(steps ~ interval + daytype, imputeDf, mean)

xyplot(steps ~ interval | daytype, groups = daytype,
	data = dailySteps, type = "l",
	layout = c(1, 2),
	ylab = "Step Count",
	xlab = "Interval",
	main = "Panel plot of weekday vs. weekend day step activity")
```

![](PA1_template_files/figure-html/panelplot-weekday-vs-weekend-1.png)<!-- -->
