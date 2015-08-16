##
## loading and preprocessing the data
##
library(reshape2)
library(ggplot2)
library(dplyr)
temp.f <- "./repdata-data-activity.zip"
if (!file.exists(temp.f))
{
    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileURL, destfile = temp.f, method = "curl")
    rm(fileURL)
}
files <- unzip(temp.f, list = TRUE)
activity <- read.csv(unz(temp.f, files[1, 1]), stringsAsFactors = FALSE)
activity$date <- as.POSIXct(strptime(activity$date, format = "%Y-%m-%d"))
molten <- melt(activity, id = c("date", "interval"), variable.name = "steps")

##
## What is mean total number of steps taken per day?
##
dailySteps  <- dcast(molten, date ~ steps, sum, na.rm = TRUE)
hist(dailySteps$steps, breaks = 10)
meanSteps   <- mean(dailySteps$steps)
medianSteps <- median(dailySteps$steps)

##
## What is the average daily activity pattern?
##
activityPattern <- dcast(molten, interval ~ steps, mean, na.rm = TRUE)
g1 <- ggplot(activityPattern, aes(x = interval, y = steps)) + geom_line()
print(g1)
maxInterval <- activityPattern[which.max(activityPattern$steps), 1]

##
## Impute the missing values.
##
wideImputed <- dcast(molten, date ~ interval)
for (i in 2:ncol(wideImputed))
{
    wideImputed[is.na(wideImputed[, i]), i] <- mean(wideImputed[, i],
                                                      na.rm = TRUE)
}
imputedDailySums   <- rowSums(wideImputed[2:289])
hist(imputedDailySums, breaks = 10)
imputedMeanSteps   <- mean(imputedDailySums)
imputedMedianSteps <- median(imputedDailySums)
##
## Are there differences in activity patterns between weekdays and weekends?
##
moltenImputed <- melt(wideImputed, id = "date")
colnames(moltenImputed) = c("date", "interval", "steps")
moltenImputed <- melt(moltenImputed, id = c("date", "interval"),
                      variable.name = "steps")
moltenImputed <- mutate(moltenImputed,
                        weekday = ifelse(weekdays(date) == "Saturday" |
                                             weekdays(date) == "Sunday",
                                         "weekend", "weekday"))
activityPatternImputed <- dcast(moltenImputed, weekday + interval ~ steps, mean)
g2 <- ggplot(activityPatternImputed, aes(x = interval, y = steps)) +
    geom_line() +
