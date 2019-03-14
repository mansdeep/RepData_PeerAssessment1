---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

##         Information      

Author         :  <span style ="color :green"> Manas Chatterjee  </span> <br>
Date           :  March, 2019 <br>
Purpose        :  Week 2 project on Reproducible Research in Johns Hopkins University Data Science Courses 


## Loading and preprocessing the data

Install packages if not available   


```r
# This block of code checks if the package is installed, and if not, installs it
list.of.packages <- c("dplyr", "tidyr", "RColorBrewer","ggthemes","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
```

Load packages in the environment  


```r
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggthemes)
library(ggplot2)
```

Check if the file exists. If not download the file from given link 


```r
temp <- tempfile()
if (!file.exists('./data/files/activity.csv')) {
        print(paste0("You did not have the file; downloaded.... "))        
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp,exdir = './data/files')
} 
```

Read the data into a dataframe `activity` 


```r
activity <- read.csv('./data/files/activity.csv', sep = ",",na.strings = "NA" )
# Now, use `str` to look at the structure of the dataframe
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day? 


```r
per_day <- activity%>%
    na.omit()%>%                            # removing the na data
    group_by(date)%>%
    summarise(total.steps=sum(steps),
              average.steps = mean(steps))
head(per_day)                               # This dataset contains MEAN total number of steps taken each day
```

```
## # A tibble: 6 x 3
##   date       total.steps average.steps
##   <fct>            <int>         <dbl>
## 1 2012-10-02         126         0.438
## 2 2012-10-03       11352        39.4  
## 3 2012-10-04       12116        42.1  
## 4 2012-10-05       13294        46.2  
## 5 2012-10-06       15420        53.5  
## 6 2012-10-07       11015        38.2
```
Sneak peak into the dataset
 

```r
g <- ggplot(per_day,aes(total.steps))
g + geom_histogram(binwidth = 1000, aes(fill=..count..))+
        stat_function(fun = dnorm, colour = "red")+
        scale_y_continuous(breaks=c(0,4,8,12))+
        scale_colour_economist()+
        labs(x="Steps taken in a single day",y="Count",title="Histogram of number of steps taken in a day ")
```

![](PA1_template_files/figure-html/historgram_plot-1.png)<!-- -->
<br>Overall MEAN and MEDIAN


```r
# Overall mean - MEAN of average number of steps taken in a day
print(paste0(as.integer(mean(per_day$total.steps))))
```

```
## [1] "10766"
```

```r
# Overall median - MEDIAN of average number of steps taken in a day
print(paste0(median(per_day$total.steps)))
```

```
## [1] "10765"
```

## What is the average daily activity pattern?

Finding MEAN steps for each interval


```r
interval_activity <- activity%>%
    na.omit()%>%                            # removing the na data
    group_by(interval)%>%
    summarise(average.steps = mean(steps))
```

Time series plot by interval


```r
l <- ggplot(interval_activity,aes(x=interval,y=average.steps))
l + geom_line(aes(colour=average.steps))+
#scale_x_continuous(breaks=seq(0,2500,by=250))+
#theme_economist()+
geom_text(aes(label=ifelse(interval_activity$average.steps == max(interval_activity$average.steps),
                           as.numeric(interval_activity$interval),'')))+
        labs(x="5 minute intervals",y="Average Steps",
             title="The busiest 5-minute interval")
```

![](PA1_template_files/figure-html/max_interval-1.png)<!-- -->
<br>Result from the plot


```r
print(paste0("The 5-minute interval with the maximum average steps is ",
interval_activity$interval[interval_activity$average.steps == max(interval_activity$average.steps)],
' with number of average steps ',as.integer(max(interval_activity$average.steps))))
```

```
## [1] "The 5-minute interval with the maximum average steps is 835 with number of average steps 206"
```
## Imputing missing values

Checking for NA or missing values


```r
sapply(activity,function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```
Filling in missing values with MEAN by the same interval


```r
new_activity <- activity%>%
    group_by(interval)%>%
    mutate(steps = ifelse(is.na(steps),mean(steps,na.rm=TRUE),steps))
    
filled_activity <- new_activity%>%
    group_by(date)%>%
    summarise(total.steps=sum(steps),
              average.steps = mean(steps))
    
    
head(filled_activity)                       # Summarized steps by day after filling in missing values
```

```
## # A tibble: 6 x 3
##   date       total.steps average.steps
##   <fct>            <dbl>         <dbl>
## 1 2012-10-01      10766.        37.4  
## 2 2012-10-02        126          0.438
## 3 2012-10-03      11352         39.4  
## 4 2012-10-04      12116         42.1  
## 5 2012-10-05      13294         46.2  
## 6 2012-10-06      15420         53.5
```

Histogram with new filled data (imputed).


```r
g <- ggplot(filled_activity,aes(total.steps))
g + geom_histogram(binwidth = 1000, aes(fill=..count..))+
        stat_function(fun = dnorm, colour = "red")+
        labs(x="Steps taken in a single day",y="Count",
             title="Histogram of number of steps taken in a day (after filling in missing data)")
```

![](PA1_template_files/figure-html/filled_hist-1.png)<!-- -->

<br>Overall MEAN and MEDIAN


```r
# Overall mean - MEAN of average number of steps taken in a day
print(paste0(as.integer(mean(filled_activity$total.steps))))
```

```
## [1] "10766"
```

```r
# Overall median - MEDIAN of average number of steps taken in a day
print(paste0(as.integer(median(filled_activity$total.steps))))
```

```
## [1] "10766"
```

## Are there differences in activity patterns between weekdays and weekends?

Convert date column into Date type


```r
new_activity$date <- as.Date(new_activity$date)
weekdays1 <- c('Monday',"Tuesday","Wednesday","Thursday","Friday")
new_activity$Weekday <- c('weekend','weekday')[(weekdays(new_activity$date) %in% weekdays1)+ 1L]
head(new_activity)
```

```
## # A tibble: 6 x 4
## # Groups:   interval [6]
##    steps date       interval Weekday
##    <dbl> <date>        <int> <chr>  
## 1 1.72   2012-10-01        0 weekday
## 2 0.340  2012-10-01        5 weekday
## 3 0.132  2012-10-01       10 weekday
## 4 0.151  2012-10-01       15 weekday
## 5 0.0755 2012-10-01       20 weekday
## 6 2.09   2012-10-01       25 weekday
```

Checking the factors


```r
weekdays(new_activity$date[6])
```

```
## [1] "Monday"
```

```r
filled_interval_activity <- new_activity%>%
    group_by(interval,Weekday)%>%
    summarise(average.steps = mean(steps))
head(filled_interval_activity)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [3]
##   interval Weekday average.steps
##      <int> <chr>           <dbl>
## 1        0 weekday        2.25  
## 2        0 weekend        0.215 
## 3        5 weekday        0.445 
## 4        5 weekend        0.0425
## 5       10 weekday        0.173 
## 6       10 weekend        0.0165
```
Panel plot of activity comparing weekdays and weekends


```r
m <- ggplot(filled_interval_activity,aes(x=interval,y=average.steps, fill=Weekday))
m + geom_line(aes(colour=Weekday)) +
    facet_grid(Weekday~.)+ 
    labs(x="5 minute intervals",y="Average Steps",title="The busiest 5-minute interval (Weekend vs. Weekdays)")
```

![](PA1_template_files/figure-html/final_plot-1.png)<!-- -->
