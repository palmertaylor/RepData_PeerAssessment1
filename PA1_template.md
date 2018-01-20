---
output: 
  html_document: 
    keep_md: yes
---
Reproducible Research - Course Project 1
========================================

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##### The data for this assignment can be downloaded from the course web site:

**Dataset**: Activity monitoring data [52K]
The variables included in this dataset are:

**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

**date**: The date on which the measurement was taken in YYYY-MM-DD format

**interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


```r
library(knitr)
opts_chunk$set(echo = TRUE)
```

## PART 0

### 0.1 Read in the data


```r
library(ggplot2)
library(plyr)
library(lattice)
```


```r
activity <- read.csv("activity.csv", header = TRUE)
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

### 0.2 Process/transform the data

```r
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime <- as.POSIXct(activity$date, format = "%Y-%m-%d")

tidy <- activity[!is.na(activity$steps),]
```

## PART 1

### 1.1 Total number of steps taken per day

```r
sumTable <- aggregate(activity$steps ~ activity$date, FUN = sum)
colnames(sumTable) <- c("Date", "Steps")
```

### 1.2 Histogram of total number of steps taken per day

```r
hist(sumTable$Steps, breaks = 5, col = "orange", xlab = "Steps", main = "Total number of steps per day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
dev.copy(png, file = '1.2.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

### 1.3 Mean (10,766) and median (10,765) of the total number of steps taken per day

```r
as.integer(mean(sumTable$Steps))
```

```
## [1] 10766
```

```r
as.integer(median(sumTable$Steps))
```

```
## [1] 10765
```

## PART 2

### 2.1 Time series plot of 5-min interval and avg number of steps taken across all days

```r
tidy <- activity[!is.na(activity$steps),]

intervalTable <- ddply(tidy, .(interval), summarize, Avg = mean(steps))

i <- ggplot(intervalTable, aes(x = interval, y = Avg), xlab = "Interval", ylab = "Avg number of steps")

i + geom_line() + xlab ("Interval") + ylab ("Avg number of steps") + ggtitle ("Avg number of steps per interval")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
dev.copy(png, file = '2.1.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

### 2.2 5-min interval that contains max number of steps = 835

```r
maxSteps <- max(intervalTable$Avg)

intervalTable[intervalTable$Avg == maxSteps, 1]
```

```
## [1] 835
```

## PART 3

### 3.1 Total number of missing values = 2,304

```r
nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```

### 3.2 Strategy to fill in all missing values = substituting missing steps with avg interval from the day of the week

```r
avgTable <- ddply(tidy, .(interval, day), summarize, Avg = mean(steps))

nadata <- activity[is.na(activity$steps),]

newdata <- merge(nadata, avgTable, by = c("interval", "day"))
```

### 3.3 New dataset equal to original dataset with missing data filled in

```r
missingdatafill <- newdata[,c(6,4,1,2,5)]
colnames(missingdatafill) <- c("steps", "date", "interval", "day", "DateTime")

mergeData <- rbind(tidy, missingdatafill)
```

### 3.4 Histogram of total number of steps taken each day with mean (10,821) and median (11,015)

```r
sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum)
colnames(sumTable2)<- c("Date", "Steps")

as.integer(mean(sumTable2$Steps))
```

```
## [1] 10821
```

```r
as.integer(median(sumTable2$Steps))
```

```
## [1] 11015
```

```r
hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total steps per day", col="Red")
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total steps per day", col="Blue", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("red", "blue") )
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

```r
dev.copy(png, file = '3.4.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

## PART 4

### 4.1 2-level factor variable of whether date is a "weekday" and "weekend"

```r
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```

### 4.2 Panel plot of time series plot of 5-minute interval and avg number of steps taken across all weekdays or weekends

```r
intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))

xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
       main="Avg steps per interval on type of day", 
       ylab="Avg number of steps", xlab="Interval")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)

```r
dev.copy(png, file = '4.2.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
