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
meanSteps   <- mean(dailySteps$steps)
medianSteps <- median(dailySteps$steps)
h1 <- ggplot(dailySteps, aes(x = steps)) +
    geom_histogram(binwidth = 1200, color = "dodgerblue", fill = "dodgerblue4") +
    geom_vline(aes(xintercept = meanSteps),
               linetype = "solid", color = "#990033", size = 1) +
    geom_vline(aes(xintercept = medianSteps),
               linetype = "dashed", color = "#990033", size = 1)

##
## What is the average daily activity pattern?
##
activityPattern <- dcast(molten, interval ~ steps, mean, na.rm = TRUE)
g1 <- ggplot(activityPattern, aes(x = interval, y = steps)) + geom_line()
maxInterval <- activityPattern[which.max(activityPattern$steps), 1]

##
## Impute the missing values.
##
moltenI <- molten %>% group_by(interval) %>%
    mutate(value = ifelse(is.na(value), mean(value, na.rm = TRUE), value))
dailyStepsI  <- dcast(moltenI, date ~ steps, sum, na.rm = TRUE)
meanStepsI   <- mean(dailyStepsI$steps)
medianStepsI <- median(dailyStepsI$steps)
hI <- ggplot(dailyStepsI, aes(x = steps)) +
    geom_histogram(binwidth = 1200, color = "dodgerblue", fill = "dodgerblue4") +
    geom_vline(aes(xintercept = meanStepsI),
               linetype = "solid", color = "#990033", size = 1) +
    geom_vline(aes(xintercept = medianStepsI),
               linetype = "dashed", color = "#990033", size = 1)
meanStepsI   <- mean(dailyStepsI$steps)
medianStepsI <- median(dailyStepsI$steps)

##
## Are there differences in activity patterns between weekdays and weekends?
##
moltenIWD <- moltenI %>% mutate(weekday = ifelse(weekdays(date) == "Saturday" |
                                             weekdays(date) == "Sunday",
                                         "weekend", "weekday"))
activityPatternIWD <- dcast(moltenIWD, interval + weekday ~ steps, mean, na.rm = TRUE)
g2 <- ggplot(activityPatternIWD, aes(x = interval, y = steps)) + geom_line() +
    facet_wrap(~weekday, nrow = 2)



