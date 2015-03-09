#set working directory to location where data file is placed
setwd("C:/Users/meet/Desktop/Exploratory_Course/Reproducible_Research")

#reading data into R
data <- read.csv("activity.csv")

#summary data
colnames(data)
head(data)


##
#What is mean total number of steps taken per day?
##

#process data 
process_data <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)

#Calculate the total number of steps taken per day
process_data
hist(process_data$steps)

#Calculate and report the mean and median of the total number of steps taken per day
mean(process_data$steps)
median(process_data$steps)

##
#What is the average daily activity pattern?
##
steps_interval <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)
steps_interval

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(steps ~ interval, data = steps_interval, type = "l")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
steps_interval[which.max(steps_interval$steps), ]$interval



##
#Imputing missing values
##

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(data$steps))

#Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the mean/median 
#for that day, for the mean for that 5-minute interval, etc.
interval2steps <- function(interval) {
  stepsInterval[stepsInterval$interval == interval, ]$steps}

##I used a strategy for filing in all of the missing values with the mean 
##for that 5-minute interval. 


#Create a new dataset that is equal to the original dataset but with the missing data filled in.
activityFilled <- data  # Make a new dataset with the original data

count = 0  # Count the number of data filled in

for (i in 1:nrow(activityFilled)) {
  if (is.na(activityFilled[i, ]$steps)) {
    activityFilled[i, ]$steps <- interval2steps(activityFilled[i, ]$interval)
    count = count + 1
  }
}

count
##  Total 2304 NA values were filled.

#Make a histogram of the total number of steps taken each day and Calculate and 
#report the mean and median total number of steps taken per day. Do these values differ 
#from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?
totalSteps2 <- aggregate(steps ~ date, data = activityFilled, sum)
totalSteps2

hist(totalSteps2$steps)
mean(totalSteps2$steps)
median(totalSteps2$steps)




##
#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.
##
activityFilled$day = ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6 == 
                              0, "weekend", "weekday")

# For Sunday and Saturday : weekend, Other days : weekday
activityFilled$day = factor(activityFilled$day, levels = c("weekday", "weekend"))

#Make a panel plot containing a time series plot
stepsInterval2 <- aggregate(steps ~ interval + day, activityFilled, mean)

library(lattice)
xyplot(steps ~ interval | factor(day), data = stepsInterval2, aspect = 1/2, 
       type = "l")





