##Opening up the activity.csv data set, I have it saved in a directory
mydata <- read.csv(."/ReproducibleResearch/activity.csv")
##Calculating the total daily steps using tapply
dailysteps <- tapply(mydata$steps, mydata$data, sum)
##Creating a histogram of the number of steps per day
hist(dailysteps, xlab = "Number of Steps", main = "Steps per Day"
##Calculating the mean and median of the total number of steps per day
MeanDaily <- mean(dailysteps, na.rm = TRUE)
MedianDaily <- median(dailysteps, na.rm = TRUE)
##Have it print out "The mean daily steps was: MeanDaily; The median daily steps was: MedianDaily.
##For average daily activity pattern we want to make a time series plot of the 5-minute interval and the average
##number of steps taken, averaged across all days
StepsperInterval <- tapply(mydata$steps, mydata$interval, mean, na.rm=TRUE)
plot(as.numeric(names(StepsperInterval)),
     StepsperInterval,
     xlab = "Interval",
     ylab = "Steps",
     main = "Average Daily Activity Pattern",
     type = "l")
##Which 5-minute interval, on average across all days in the dataset, contains the max number of steps?
maxInterval <- names(sort(StepsperInterval, decreasing = TRUE)[1])
maxSteps <- sort(StepsperInterval, decreasing = TRUE)[1]
##Output: the interval associated with the max amount of activity is maxInterval, at maxSteps steps.
##Calculate and report the total number of missing values in the dataset (i.e. total number fo rows with NAs)
TotNAs <- sum(is.na(mydata$steps))
##Output: There are TotNAs missing values in this data set.
##Devise a strategy for filling in all of the missing values in the dataset. I will be using the mean steps per interval for missing data values
##splitting up the activity data by interval
activity_split <- split(mydata, mydata$interval)
##fill-in the missing data in each interval
for(i in 1:length(activity_split)){
  activity_split[[i]]$steps[is.na(activity_split[[i]]$steps)] <- StepsperInterval[i]
}
activity_added <- do.call("rbind",activity_split)
activity_added <- activity_added[order(activity_added$date),]
##Make a histogram of the total number of steps taken each day. 
StepsPerDay_added <- tapply(activity_added$steps,activity_added$date,sum)
hist(StepsPerDay_added, xlab ="Number of Steps", main ="Steps per Day: NAs replaced")
##Calculate and report the mean and median total number of steps taken per day
MeanDaily_added <- mean(StepsPerDay_added, na.rm =TRUE)
MedianDaily_added <- median(StepsPerDay_added, na.rm =TRUE)
##Output: The mean steps per day were MeanDaily_added, and the median steps per day were MedianDaily_added. 
##Include info about differences in the mean and median as compared to what they were before. I believe they
##both increased.
##Are there differences in activity patterns between weekdays and weekends?
##Could use the weekdays() function here. Use the dataset that contains the filled in missing data values.
##Create a new factor variable inside the dataset with 2 levels - "weekday" and "weekend" indicating the day tupe.
activity_added$day <- ifelse(weekdays(as.Date(activity_added$date))=="Saturday" | weekdays(as.Date(activity_added$date))=="Sunday", "weekend", "weekday")
##Make a panel plot containing a time series of the 5-minute intervals and the average number of steps taken,
##averaged actross all weekdays or all weekend days.
##Calculating average steps per interval for weekends
StepsPerInterval_weekend <- tapply(activity_added[activity_added$day == "weekend",]$steps, activity_added[activity_added$day == "weekend",]$interval,mean, na.rm =TRUE)
##Calculate average steps per interval for weekdays
StepsPerInterval_weekday <- tapply(activity_added[activity_added$day == "weekday",]$steps, activity_added[activity_added$day == "weekday",]$interval, mean, na.rm =TRUE)
##Creating a 2 panel plot
par(mfrow=c(1,2))
##Plotting weekday activity
plot(as.numeric(names(StepsPerInterval.weekday)),
     StepsPerInterval_weekday,
     xlab = "Interval",
     ylab = "Steps",
     main = "Activity Pattern: Weekdays",
     type = "l")
##Plotting weekend activity 
plot(as.numeric(names(StepsPerInterval_weekend)),
     StepsPerInterval_weekend,
     xlab = "Interval",
     ylab = "Steps",
     main = "Activity Pattern: Weekends",
     type = "l")





