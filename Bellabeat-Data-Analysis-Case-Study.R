install.packages("tidyverse")
install.packages("lubridate")

library(tidyverse)
library("lubridate")


#Check if the observations of the three datasets (dailySteps, dailyCalories, dailyIntensities) are includes in dailyActivity dataset
dailyActivity <- read_csv("dailyActivity_merged.csv")
id_activity <- unique(dailyActivity$Id)
dailySteps <- read_csv("dailySteps_merged.csv")
id_steps <- unique(dailySteps$Id)
all.equal(id_activity, id_steps)
dailyCalories <- read_csv("dailyCalories_merged.csv")
all.equal(unique(dailyCalories$Id), id_activity)

weight <- read_csv("weightLogInfo_merged.csv")
n_distinct(weight$Id)

rm(id_activity, dailySteps, id_steps, dailyCalories, weight)

hourlySteps <- read_csv("hourlySteps_merged.csv")
sleepDay <- read_csv("sleepDay_merged.csv")

#Check if there are any NA values
any(is.na(dailyActivity))
any(is.na(hourlySteps))
any(is.na(sleepDay))

#Check if there are any duplicated values
any(duplicated(dailyActivity))
any(duplicated(hourlySteps))
any(duplicated(sleepDay))

#Remove duplicated values from sleepDay
sleepDay <- sleepDay[!duplicated(sleepDay), ]

#Change the format and type of ActivityDate and create a new column for the weekdays
str(dailyActivity)
dailyActivity$ActivityDate <- format(as.Date(dailyActivity$ActivityDate, "%m/%d/%y"), "%d/%m/%y")
dailyActivity$ActivityDate <- as.Date(dailyActivity$ActivityDate, "%d/%m/%y")
dailyActivity$WeekDay <- weekdays(dailyActivity$ActivityDate)

#Merge dailyActivity with SleepDay
sleepDay$ActivityDate <- format(as.Date(sleepDay$SleepDay, "%m/%d/%y"), "%d/%m/%y")
sleepDay$ActivityDate <- as.Date(sleepDay$ActivityDate, "%d/%m/%y")
data <- merge(dailyActivity, sleepDay, by=c("Id", "ActivityDate"), all=TRUE)
any(duplicated(data))

#Check if column TotalTimeInBed is greater than TotalMinuteAsleep
all(with(data, data$TotalMinutesAsleep <= data$TotalTimeInBed), na.rm=TRUE)

#Cut to two decimal digits the data frame columns VeryActiveDistance,	ModeratelyActiveDistance,	LightActiveDistance and TotalDistance
data[, 'VeryActiveDistance'] = round(data[, 'VeryActiveDistance'], 2)
data[, 'ModeratelyActiveDistance'] = round(data[, 'ModeratelyActiveDistance'], 2)
data[, 'LightActiveDistance'] = round(data[, 'LightActiveDistance'], 2)
data[, 'SedentaryActiveDistance'] = round(data[, 'SedentaryActiveDistance'], 2)
data$TotalDistance = data$VeryActiveDistance + data$ModeratelyActiveDistance + data$LightActiveDistance + data$SedentaryActiveDistance

#Order the dataframe by the Weekday column from "Δευτέρα" to "Κυριακή"
weekday_order <- c("Δευτέρα", "Τρίτη", "Τετάρτη", "Πέμπτη", "Παρασκευή", "Σάββατο", "Κυριακή")
data <- data %>% arrange(match(WeekDay, weekday_order))
data$WeekDay[data$WeekDay == 'Δευτέρα'] <- 'Monday'
data$WeekDay[data$WeekDay == 'Τρίτη'] <- 'Tuesday'
data$WeekDay[data$WeekDay == 'Τετάρτη'] <- 'Wednesday'
data$WeekDay[data$WeekDay == 'Πέμπτη'] <- 'Thursday'
data$WeekDay[data$WeekDay == 'Παρασκευή'] <- 'Friday'
data$WeekDay[data$WeekDay == 'Σάββατο'] <- 'Saturday'
data$WeekDay[data$WeekDay == 'Κυριακή'] <- 'Sunday'

write_csv(data, "data.csv")

#Change the format of ActivityHour of hourlySteps and get the hour into a new column
str(hourlySteps)
hourlySteps$ActivityHour <- parse_date_time(hourlySteps$ActivityHour, "%m/%d/%y %I:%M:%S %p")
hourlySteps$Hour <- format(as.POSIXct(hourlySteps$ActivityHour), format = "%H")
write_csv(hourlySteps, "hourlySteps_merged.csv")
