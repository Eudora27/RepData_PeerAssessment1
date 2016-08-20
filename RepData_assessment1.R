# Load Packages and Functions
library(dplyr)
library(ggplot2)
library(doBy)

# Load the data
df <- read.csv('/Users/wanranli/Desktop/DataScience-Coursera/reproducible_research/RepData_PeerAssessment1/activity.csv', stringsAsFactors = FALSE)


# Q1: What is mean total number of steps taken per day?
# 1. Calculate the total number of steps taken per day
totalByDate<-tapply(df$steps, df$date, sum, na.rm = TRUE, simplify = TRUE)

# 2. Make a histogram of the total number of steps taken each day

hist(totalByDate, xlab="Number of steps taken each day", 
     main = "Total number of steps taken each day")

# 3. Calculate and report the mean and median of the total number of steps taken per day
meanByDate<-as.data.frame(tapply(df$steps, df$date, mean, na.rm = TRUE, simplify = TRUE))
names(meanByDate)<-"step_mean"
medianByDate<-as.data.frame(tapply(df$steps, df$date, median, na.rm = TRUE, simplify = TRUE))
names(medianByDate)<-"step_median"

# Q2: What is the average daily activity pattern?
# 1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and
     #the average number of steps taken, averaged across all days (y-axis)

meanByInterval<-as.data.frame(tapply(df$steps, df$interval, mean, na.rm = TRUE))
meanByInterval$interval <- row.names(meanByInterval)
names(meanByInterval) <- c("step_mean", "interval")
plot(meanByInterval$interval, meanByInterval$step_mean, type="l", 
     xlab= "5-minute interval", 
     ylab= "Average number of steps taken", col="red", lwd=2, 
     main = "Average daily activity pattern")

# 2. Which 5-minute interval, on average across all the days in the dataset, 
     #contains the maximum number of steps?
sort(meanByInterval$step_mean, decreasing = TRUE)[1]

# Q3: Imputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰)
  #The presence of missing days may introduce bias into some calculations or summaries of the data.

# 1. Calculate and report the total number of missing values 
  #in the dataset (i.e. the total number of rows with 𝙽𝙰s)
sum(!complete.cases(df))

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
  #The strategy does not need to be sophisticated. For example, 
  # you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

newStep <- NULL
for (row in 1:nrow(df)) {
  if(is.na(df[row, 1])){
    newStep <- c(newStep, meanByInterval[match(df[row, 3], meanByInterval$interval), "step_mean"])
  } else{
    newStep <- c(newStep, df[row, 1])
  }
}

# 3. Create a new dataset that is equal to the original dataset but with 
# the missing data filled in.
dfImpute <- cbind(newStep, df[,c(2,3)])

# 4. Make a histogram of the total number of steps taken each day and 
# Calculate and report the mean and median total number of steps taken 
# per day. Do these values differ from the estimates from the first part 
# of the assignment? What is the impact of imputing missing data on the 
# estimates of the total daily number of steps?


totalByDate2<-tapply(dfImpute$newStep, dfImpute$date, sum, na.rm = TRUE, simplify = TRUE)
hist(totalByDate2, xlab="Number of steps taken each day", 
     main = "Total number of steps taken each day")

meanByDate2<-as.data.frame(tapply(dfImpute$newStep, dfImpute$date, mean, na.rm = TRUE, simplify = TRUE))
names(meanByDate2)<-"step_mean"
medianByDate2<-as.data.frame(tapply(dfImpute$newStep, dfImpute$date, median, na.rm = TRUE, simplify = TRUE))
names(medianByDate2)<-"step_median"

# After inputation using mean values, the data is centralised

# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. Use the dataset 
# with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with two levels – 
# “weekday” and “weekend” indicating whether a given date is a 
# weekday or weekend day.
dfImpute$date <- as.Date(dfImpute$date)
dfImpute$dayType <- ifelse(weekdays(dfImpute$date) %in% c("Saturday","Sunday"), 
                           "weekend", "weekday")

# 2. Make a panel plot containing a time series plot (i.e. type = "l") 
# of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). 
# See the README file in the GitHub repository to see an example of 
# what this plot should look like using simulated data.

meanByInterval2 <- summaryBy(formula = newStep ~ interval + dayType, 
                             data = dfImpute, FUN = mean)

ggplot(meanByInterval2, aes(x = interval, y = newStep.mean)) + 
  geom_line(aes(color = dayType)) + 
  facet_grid(dayType ~ ., scales = "free_y") + 
  theme(legend.position = "none") +
  labs(x="5-minute interval",y="Average number of steps taken") +
  ggtitle("Average daily activity pattern")
