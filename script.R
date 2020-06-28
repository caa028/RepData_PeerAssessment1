
# first we check if the source data file exists and has been un-zipped
if (("activity.zip" %in% dir()) & !("activity.csv" %in% dir())) {
  # and un-zip, if necessary
  unzip("activity.zip")
}

# then read the content of the CSV file into a data frame
activity <- read.table("activity.csv",
                       header = TRUE,
                       sep = ",",
                       na.strings = "NA",
                       colClasses = c("integer","Date","integer"))

# we will use dplyr and ggplot2 libraries
library(dplyr)
library(ggplot2)

# summarize the number of steps per day
daily <- activity %>%
  group_by(date) %>%
  summarise(steps = sum(steps, na.rm = TRUE))

# show summary of the daily activity (mean, median)
summary(daily$steps)

# plot a histogram
ggplot(data = daily, aes(x = steps)) +
  geom_histogram(color="black", fill="white") +
  labs(title = "Number of steps taken each day")

# average daily activity pattern
avgDaily <- activity %>%
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm = TRUE))

# plot a time series
ggplot(data = avgDaily, aes(x = interval, y = steps)) +
  geom_line(color="black") +
  labs(title = "Average daily activity pattern")

# find the most active interval
mostActive <- avgDaily[[which.max(avgDaily$steps),1]]

# calculate the number of missing values in the dataset
sum(is.na(activity))

# impute missing values by substituting with average daily activity
patched <- activity %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps),
                        as.integer(avgDaily[[which(avgDaily$interval == interval), 2]]),
                        steps)) %>%
  ungroup()

# summarize the number of steps per day
dailyPatched <- patched %>%
  group_by(date) %>%
  summarise(steps = sum(steps, na.rm = TRUE))

# show summary of the daily activity (mean, median)
summary(dailyPatched$steps)

# plot a histogram
ggplot(data = dailyPatched, aes(x = steps)) +
  geom_histogram(color="black", fill="white") +
  labs(title = "Number of steps taken each day (imputed data)")

# add new variable indicating whether a given date is a weekday or weekend
enhanced <- patched %>%
  mutate(day = as.factor(weekdays(date)), weekday = as.factor(ifelse(day %in% c("Saturday", "Sunday"), "weekend", "weekday")))

# average daily enhanced activity pattern
avgDailyEnh <- enhanced %>%
  group_by(interval, weekday) %>%
  summarise(steps = mean(steps, na.rm = TRUE))

# plot a time series
ggplot(data = avgDailyEnh, aes(x = interval, y = steps)) +
  geom_line(color="black") +
  facet_grid(rows = vars(weekday)) +
  labs(title = "Average daily activity pattern")
