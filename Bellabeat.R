#installing required packages
install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)

#Loading the necessary data
daily_activity <- read.csv('dailyActivity_merged.csv')
daily_calories <- read.csv('dailyCalories_merged.csv')
daily_steps <- read.csv('dailySteps_merged.csv')
sleep <- read.csv('sleepDay_merged.csv')
hourly_calories <- read.csv('hourlyCalories_merged.csv')
hourly_intensities <- read.csv('hourlyIntensities_merged.csv')

#Renaming certain columns for easier merge

rename(daily_steps, ActivityDate = ActivityDay ) -> daily_steps
rename(daily_calories , ActivityDate = ActivityDay) -> daily_calories

#merging daily_activity and daily_step

merge(daily_activity,daily_steps) -> merge_1
merge(merge_1 ,daily_calories ) -> daily_complete

#Making sure all columns are in the right format

daily_complete$ActivityDate <- as.Date(daily_complete$ActivityDate , "%m/%d/%Y")
daily_complete$Id <- as.character(daily_complete$Id)

sleep$Id <- as.character(sleep$Id)
sleep$SleepDay <- parse_date_time(sleep$SleepDay , "%m/%d/%y%H:%M:%S")

hourly_calories$Id <- as.character(hourly_calories$Id)
hourly_calories$ActivityHour <- parse_date_time(hourly_calories$ActivityHour , "%m/%d/%y%I:%M:%S%p")
hourly_calories$date <- as.Date(hourly_calories$ActivityHour , format = "%m/%d/%y")
hourly_calories$time <- format(as.POSIXct(hourly_calories$ActivityHour), format = "%H:%M:%S" )
sleep$date <- as.Date(sleep$SleepDay , format = "%m/%d/%y")

hourly_intensities$Id <- as.character(hourly_intensities$Id)
hourly_intensities$ActivityHour <- parse_date_time(hourly_intensities$ActivityHour , "%m/%d/%y%I:%M:%S%p")
hourly_intensities$date <- as.Date(hourly_intensities$ActivityHour , format = "%m/%d/%y")
hourly_intensities$time <- format(as.POSIXct(hourly_intensities$ActivityHour), format = "%H:%M:%S" )

#Checking for any null and duplicated value

is.null(daily_complete)

is.null(sleep)

sum(duplicated(daily_complete))
sum(duplicated(sleep))

sleep %>% distinct() %>% drop() -> sleep

sum(duplicated(sleep))

#Taking a look at the data

daily_complete %>% select(TotalSteps,TotalDistance, Calories) %>% summary()

daily_complete %>% select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes,SedentaryMinutes) %>% summary()

daily_complete %>% select(Calories, TotalSteps) %>% summary()

#Data Visualization

#Let's see the total time spend in bed vs total mintue asleep

ggplot(data = sleep , aes(x = TotalTimeInBed , y = TotalMinutesAsleep
)) + geom_point() + geom_smooth() + labs(title =  "Total minute asleep vs Total minute in bed" )

#Let's take a look at the number of Calories burned vs Total Steps taken

ggplot(data = daily_complete, aes(x = TotalSteps , y = Calories)) + geom_point(
) + geom_smooth() + labs( title = "Calories burned vs total Step taken")

#Let's see what more we can find out about the users - Average number of Calories burned vs Time(Hourly)

cal_new <- hourly_calories %>%
  group_by(time) %>%
  summarise(cal_mean = mean(Calories))

ggplot(data = cal_new, aes(x = time , y = cal_mean)) + geom_col( fill = "blue"
                                                                   
) + theme(axis.text.x = element_text(angle = 90)) + labs(
  title = "Average number of calories burned vs Time" , y = "mean calories"
)

#Average intensity vs Time(Hourly)

int_new <- hourly_intensities %>%
  group_by(time) %>%
  summarise(int_mean = mean(TotalIntensity))

ggplot(data = int_new, aes(
  x = time , y = int_mean
)) + geom_col( fill = "green") + theme(
  axis.text.x = element_text(angle = 90)
) + labs(
  title = "Average intensity vs Time" , y = "mean intensity"
)
