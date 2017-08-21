---
title: "Programming Assignment"
author: "Rapolu Yashwanth Kumar"
date: "August 21, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


### Loading and preprocessing the data
Load data from directory
```{r}
setwd('C:/Users/yashwanth_2/Desktop/Course')
data = read.csv('activity.csv',colClasses = "character")
data$steps = as.numeric(data$steps)
```

Preprocessing:
activity contains rows of data with out NA
```{r}
activity = data[!is.na(data$steps),]
```

### What is mean total number of steps taken per day?

totalSteps contains sum of steps for each day
```{r , echo=TRUE}
totalSteps = tapply(activity$steps,activity$date,sum,na.rm=TRUE)
hist(totalSteps,main="Histogram of total number of steps each day",xlab="Total Step per day")
```

mean of total number of steps per day
```{r , echo=TRUE}
mean(totalSteps)
```

median of total number of steps per day
```{r , echo=TRUE}
median(totalSteps)
```


### What is the average daily activity pattern?

Average number of steps for 5-minute interval
```{r, echo=TRUE}
AvgSteps = tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
```

Below plot shows Time Series of 5 minute interval(x-axis) and the average number of steps taken averaged across all days(y-axis)
```{r, echo=TRUE}
plot(AvgSteps,type='l',xlab="5-min interval",main='Averge Steps Pattern')
```

5-minute interval 835 contains the maximum number of steps
```{r, echo=TRUE}
AvgSteps[which(AvgSteps==max(AvgSteps))]
```





### Imputing missing values

Total number of missing values
```{r, echo=TRUE}
sum(is.na(data$steps))
```

Filling of missing values by mean for that 5 minute interval
```{r, echo=TRUE}
logic = is.na(data$steps)
j=1
newData = data
for(i in logic){
  if(i){
    newData[j,1] = AvgSteps[as.character(data[j,3])]
  }
  j=j+1
}

```

newData contains original data along with missing data filled in.

histogram of the total number of steps taken each day
```{r, echo=TRUE}
newData$steps = as.numeric(newData$steps)
newTotalSteps = tapply(newData$steps,newData$date,sum,na.rm=TRUE)
hist(newTotalSteps)
```

mean number of steps per day
```{r, echo=TRUE}
mean(newTotalSteps)
```

median number of steps per day
```{r, echo=TRUE}
median(newTotalSteps)
```


### Are there differences in activity patterns between weekdays and weekends?

Column day corresponding day of the week and in foctor column contains 1 and 0 where 1 equal to weekend and 0 equal to weekday 
```{r, echo=TRUE}
newData$date = as.Date(newData$date)
newData[,"day"] = weekdays(newData$date)
for(i in 1:dim(newData)[1]){
  if(newData[i,4]=="Sunday"||newData[i,4]=="Saturday"){
    newData[i,"factor"]=1
  }else{
    newData[i,"factor"]=0
  }
}
```

Panel Plot containg time series of  5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r, echo=TRUE}
weekDayNewData = newData[which(newData[,"factor"]==0),]
weekendNewData = newData[which(newData[,"factor"]==1),]
weekDayAvgSteps = tapply(weekDayNewData$steps,weekDayNewData$interval,mean,na.rm=TRUE)
weekendAvgSteps = tapply(weekendNewData$steps,weekendNewData$interval,mean,na.rm=TRUE)

par(mfrow=c(1,2))
plot(weekDayAvgSteps,type='l',col='red',main="Weekday",xlab='interval')
plot(weekendAvgSteps,type='l',col='red',main="weekend",xlab='interval')

```

