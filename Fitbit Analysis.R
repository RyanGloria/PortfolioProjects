getwd()
setwd("/Users/Ryan/OneDrive/Desktop/bellabeat/data")

install.packages("tidyverse")

library(tidyverse)
library(lubridate)

#Uploading FitBit Data
dailyActivity = read_csv("dailyActivity_merged.csv")
heartrate = read_csv("heartrate_seconds_merged.csv")
hourlyCalories = read_csv("hourlyCalories_merged.csv")
hourlyIntensities = read_csv("hourlyIntensities_merged.csv")
hourlySteps = read_csv("hourlySteps_merged.csv")
sleepDay = read_csv("sleepDay_merged.csv")
weight = read_csv("weightLogInfo_merged.csv")

#Looking at data and inspecting columns
head(dailyActivity)
colnames(dailyActivity)
head(heartrate)
colnames(heartrate)
head(hourlyCalories)
colnames(hourlyCalories)
head(hourlyIntensities)
colnames(hourlyIntensities)
head(hourlySteps)
colnames(hourlySteps)
head(sleepDay)
colnames(sleepDay)
head(weight)
colnames(weight)

#dailyActivity has ActivityDate column as wrong data type
#Changing to date datatype
dailyActivity$ActivityDate = as.Date(dailyActivity$ActivityDate, "%m/%d/%Y")

#About to join data frames involving days
#Checking number of participants in each data frame
n_distinct(dailyActivity$Id)
n_distinct(sleepDay$Id)

#dailyActivity has more participants, will use full join to include all participants from each data frame
dailyMerged = merge(dailyActivity, sleepDay, by.x = c("Id","ActivityDate"), by.y = c("Id","SleepDay"), all = TRUE)

#Checking new data frame
head(dailyMerged,25)

#Looking at summary statistics of new data frame
dailyMerged %>%
select(
TotalSteps,
TotalDistance,
Calories,
TotalMinutesAsleep,
TotalTimeInBed
) %>%
summary()

#Creating column for day of the week to better analyze data
dailyMerged$Day = weekdays(dailyMerged$ActivityDate)

#About to join data frames involving hours
#Checking number of participants in each data frame
n_distinct(hourlyCalories$Id)
n_distinct(hourlyIntensities$Id)
n_distinct(hourlySteps$Id)

#All data frames have same amount of participants
#Merging data frames together
hourlyMerged = merge(hourlyCalories, merge(hourlyIntensities, hourlySteps, by.x = c("Id","ActivityHour"), by.y = c("Id","ActivityHour")), by.x = c("Id","ActivityHour"), by.y = c("Id","ActivityHour"))

#Inspecting new data frame
head(hourlyMerged,25)
nrow(hourlyMerged)

#Looking at summary statistics of new data frame
hourlyMerged %>%
select(
Calories,
TotalIntensity,
AverageIntensity,
StepTotal
) %>%
summary()


#Creating column for day of the week to better analyze data
hourlyMerged$Day = weekdays(hourlyMerged$ActivityHour)

#Creating column for hours to better analyze data
hourlyMerged$Hour = hour(hourlyMerged$ActivityHour)

#Creating column for time of day to better analyze data
hourlyMerged = hourlyMerged %>%
	mutate(TimeOfDay = case_when(
	Hour < 6 ~ "Night",
	Hour >= 6 & Hour < 12 ~ "Morning",
	Hour >= 12 & Hour < 18 ~ "Afternoon",
	Hour >= 18 & Hour < 24 ~ "Evening",
	))


head(dailyMerged, 25)

#Renaming column names to match other columns
dailyMerged = rename(dailyMerged,
ModeratelyActiveMinutes = "FairlyActiveMinutes",
LightlyActiveDistance = "LightActiveDistance",
SedentaryDistance = "SedentaryActiveDistance"
)

#Dropping columns that are unnecessary
dailyMerged = dailyMerged %>%
select(-c(TrackerDistance,LoggedActivitiesDistance,TotalSleepRecords))



head(
dailyMerged %>%
group_by(ActivityDate)%>%
summarise(n())%>%
arrange(ActivityDate)
,20)

head(
dailyMerged %>%
group_by(Id)%>%
summarise(n(),mean(TotalSteps))%>%
arrange(Id)
,20)

head(
dailyMerged %>%
group_by(Day)%>%
summarise(mean(TotalSteps))%>%
arrange(Day)
,20)

#Inspecting participants per dataset
n_distinct(sleepDay$Id)
n_distinct(dailyMerged$Id)
n_distinct(heartrate$Id)
n_distinct(weight$Id)

sleep2 = (
sleepDay %>%
group_by(Id) %>%
summarise(n(), 
num1 = sum(n() > 0 & n() < 11),
num2 = sum(n() > 10 & n() < 21),
num3 = sum(n() > 20))
)

sum(sleep2$num1)
sum(sleep2$num2)
sum(sleep2$num3)

#Plotting daily data to check for any correlation
#Total steps vs total distance
ggplot(dailyMerged, aes(x=TotalSteps,y=TotalDistance)) + geom_point()

#Total steps vs calories
ggplot(dailyMerged, aes(x=TotalSteps,y=Calories)) + geom_point()

#Total distance vs calories
ggplot(dailyMerged, aes(x=TotalDistance,y=Calories)) + geom_point()

#Total steps vs total minutes asleep
ggplot(dailyMerged, aes(x=TotalSteps,y=TotalMinutesAsleep)) + geom_point()

#Total distance vs total minutes asleep
ggplot(dailyMerged, aes(x=TotalDistance,y=TotalMinutesAsleep)) + geom_point()

#Total calories vs total minutes asleep
ggplot(dailyMerged, aes(x=TotalMinutesAsleep,y=Calories)) + geom_point()


#Plotting hourly data to check for any correlation
#Total intensity vs calories
ggplot(hourlyMerged, aes(x=TotalIntensity,y=Calories)) + geom_point()

#Total intensity vs average intensity
ggplot(hourlyMerged, aes(x=TotalIntensity,y=AverageIntensity)) + geom_point()

#Total intensity vs total steps
ggplot(hourlyMerged, aes(x=TotalIntensity,y=StepTotal)) + geom_point()

#Average intensity vs calories
ggplot(hourlyMerged, aes(x=AverageIntensity,y=Calories)) + geom_point()

#Average intensity vs total steps
ggplot(hourlyMerged, aes(x=StepTotal,y=AverageIntensity)) + geom_point()

#Total steps vs Calories
ggplot(hourlyMerged, aes(x=StepTotal,y=Calories)) + geom_point()



# Exporting csv
write.csv(dailyMerged, file = 'dailyMerged.csv')
write.csv(hourlyMerged, file = 'hourlyMerged.csv')




