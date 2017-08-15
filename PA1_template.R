#read in file
if (!file.exists('activity.csv')) {
  unzip('activity.zip')
}
activityData <- read.csv('activity.csv')

#############################PART 2############################################################
#process steps by day from data set
#grab data
stepsByDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)

#draw histogram
png("Fig1.png")

hist(stepsByDay, xlab = "Total Steps per Day", ylab = "Binned Distribution of Steps Taken",
     main = "Distribution of Daily Total Steps, filtered for missing data",
     breaks = 20, density = 1000, col = "blue")

dev.off()

#calculate the mean and median steps per day
stepsByDayMean = mean(stepsByDay)
stepsByDayMedian = median(stepsByDay)

#############################PART 3############################################################
#prepare time series plot data
intervalAverages <- tapply(activityData$steps, activityData$interval, mean, na.rm=TRUE, simplify = TRUE)
dictIntervalAverages <- data.frame(interval = as.integer(names(intervalAverages)), avg = intervalAverages)

#draw time series plot
png("Fig2.png")

with(dictIntervalAverages,
     plot(interval,
          avg,
          type = 'l',
          xlab = '5 Minute Intervals',
          ylab = 'Average Steps in Interval',
          main = 'Average Steps for Three Days Plotted Across Intervals',
          sub = 'Data filtered to ignore missing data'
          )
     )

dev.off()

#find interval with maximum number of steps
maximumSteps <- max(dictIntervalAverages$avg)
dictIntervalAverages[dictIntervalAverages$avg == maximumSteps, ]

#############################PART 4############################################################
# calculate number of missing data points in set
numMissValues <- sum(is.na(activityData$steps))

# fill in missing data by adding in the mean of the interval. Create a hash like object to do this.
dictFillIn <- activityData
keys <- is.na(dictFillIn$steps)
averageIntervalVal <- tapply(activityData$steps,activityData$interval,mean, na.rm = TRUE, simplify = TRUE)
dictFillIn$steps[keys] <- averageIntervalVal[as.character(dictIntervalAverages$interval[keys])]

#grab step and date data and plot as a histogram
correctedActivityData <- tapply(dictFillIn$steps, dictFillIn$date, sum, na.rm = TRUE, simplify = TRUE)

png("Fig3.png")

hist(correctedActivityData, col = "blue", breaks = 20, 
     xlab = "Total Steps per Day", 
     ylab = "Binned Distribution of Steps Taken", 
     main = "Distribution of Daily Total Steps, corrected for missing data", 
     sub = "Data corrected to replace missing data with averages")

dev.off()

mean(correctedActivityData)
median(correctedActivityData)

#############################PART 5############################################################
#find differences between patterns over weekends and week days
#use the date to figure out if it's a weekday or weekend
#update a new data frame for this information
dictFillIn$dayClass <- ifelse(as.POSIXlt(dictFillIn$date)$wday %in% c(0,6), 'weekend', 'weekday')

#use the data frame to prepare data for the next plot
dayClassDiff <- aggregate(steps ~ interval + dayClass, data = dictFillIn, mean)

png("Fig4.png")

xyplot(steps ~ interval | dayClass, 
       data = dayClassDiff, 
       layout = c(1,2), 
       xlab = "Intervals", 
       ylab = "Numbers of Steps", 
       type = 'l', 
       lty = 1)
dev.off()





