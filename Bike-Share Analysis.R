getwd()
setwd("/Users/Ryan/OneDrive/Desktop/TripData/UnZipped/Copy/CSV")

install.packages("tidyverse")

library(tidyverse)
library(lubridate)

#Uploading Trip Data
q2_2019 = read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 = read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 = read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 = read_csv("Divvy_Trips_2020_Q1.csv")

#Checking column names of each data frame to see if they are the same
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

#Renaming columns based on column names from q1_2019 since it has the most columns with simple names
q2_2019 = rename(q2_2019
,ride_id = "01 - Rental Details Rental ID"
,rideable_type = "01 - Rental Details Bike ID" 
,started_at = "01 - Rental Details Local Start Time"  
,ended_at = "01 - Rental Details Local End Time"  
,start_station_name = "03 - Rental Start Station Name" 
,start_station_id = "03 - Rental Start Station ID"
,end_station_name = "02 - Rental End Station Name" 
,end_station_id = "02 - Rental End Station ID"
,member_casual = "User Type")

q3_2019 = rename(q3_2019
,ride_id = "trip_id"
,rideable_type = "bikeid" 
,started_at = "start_time"  
,ended_at = "end_time"  
,start_station_name = "from_station_name" 
,start_station_id = "from_station_id" 
,end_station_name = "to_station_name" 
,end_station_id = "to_station_id" 
,member_casual = "usertype")

q4_2019 = rename(q4_2019
,ride_id = "trip_id"
,rideable_type = "bikeid" 
,started_at = "start_time"  
,ended_at = "end_time"  
,start_station_name = "from_station_name" 
,start_station_id = "from_station_id" 
,end_station_name = "to_station_name" 
,end_station_id = "to_station_id" 
,member_casual = "usertype")

#Checking the datatypes of each column for each data frame
glimpse(q2_2019)
glimpse(q3_2019)
glimpse(q4_2019)
glimpse(q1_2020)

#Changing datatypes to match q1_2019
q2_2019 = mutate(q2_2019, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type)) 

q3_2019 = mutate(q3_2019, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type)) 

q4_2019 = mutate(q4_2019, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))


# Merging all data frames into a single data frame
all_quarters = bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)


# Inspecting column names of new data frame
colnames(all_quarters)


# Removing unnecessary columns
all_quarters = all_quarters %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))


# Inspecting new data frame
head(all_quarters)
str(all_quarters)


# Checking for names of members and casual riders
table(all_quarters$member_casual)


# Renaming entries to match the 2020 naming convention
all_quarters = all_quarters %>% 
  mutate(member_casual = recode(member_casual, "Subscriber" = "member", "Customer" = "casual"))


# Making sure all ride_id rows are unique
subset(all_quarters,duplicated(ride_id))


# Creating separate columns based on date to better analyze data
all_quarters$month = format(as.Date(all_quarters$started_at), "%m")
all_quarters$day = format(as.Date(all_quarters$started_at), "%d")
all_quarters$year = format(as.Date(all_quarters$started_at), "%Y")
all_quarters$day_of_week = format(as.Date(all_quarters$started_at), "%A")
all_quarters$time_of_day = format(as.POSIXct(all_quarters$started_at), "%H")


# Adding in a column for ride duration
all_quarters$ride_duration = difftime(all_quarters$ended_at,all_quarters$started_at)


# Looking at the structure of the dataframe
str(all_quarters)


# Convert "ride_duration" to numeric to run calculations
all_quarters$ride_duration = as.numeric(as.character(all_quarters$ride_duration))


# Remove data where bikes underwent quality checks or where ride_length was negative
all_quarters_v2 = all_quarters[!(all_quarters$start_station_name == "HQ QR" | all_quarters$ride_duration<0),]


#Analysis:

# Analysis of ride_duration (In seconds)
summary(all_quarters_v2$ride_duration)


# Checking for differences between members and casual users
aggregate(all_quarters_v2$ride_duration~ all_quarters_v2$member_casual, FUN = mean)
aggregate(all_quarters_v2$ride_duration~ all_quarters_v2$member_casual, FUN = median)
aggregate(all_quarters_v2$ride_duration~ all_quarters_v2$member_casual, FUN = max)
aggregate(all_quarters_v2$ride_duration~ all_quarters_v2$member_casual, FUN = min)


# Ordering data by day of the week
all_quarters_v2$day_of_week = ordered(all_quarters_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# Changing month from text to characters
all_quarters_v2$month = month(all_quarters_v2$started_at, label = TRUE, abbr = TRUE)


# Sectioning time of day
all_quarters_v2 = all_quarters_v2 %>%
	mutate(day_section = case_when(
	time_of_day < "06" ~ "early_morning",
	time_of_day < "12" ~ "morning",
	time_of_day < "18" ~ "afternoon",
	time_of_day < "24" ~ "evening",
	))


# Checking average duration for each day of the week for members and casual users
aggregate(all_quarters_v2$ride_duration~ all_quarters_v2$member_casual + all_quarters_v2$day_of_week, FUN = mean)


# analyze data by user status and weekday
all_quarters_v3 = all_quarters_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_duration)) %>% 
  arrange(member_casual, weekday)


# Inspecting new data frame
all_quarters_v3


# Creating new data frame by user status and month
all_quarters_v4 = all_quarters_v2 %>%
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_duration)) %>% 
  arrange(member_casual, month)


# Creating new data frame by user status and time of day
all_quarters_v5 = all_quarters_v2 %>%
  group_by(member_casual, time_of_day) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_duration)) %>% 
  arrange(member_casual, time_of_day)


# Converting duration from seconds to minutes
all_quarters_v3$average_duration = all_quarters_v3$average_duration / 60
all_quarters_v4$average_duration = all_quarters_v4$average_duration / 60
all_quarters_v5$average_duration = all_quarters_v5$average_duration / 60


# Visualizing data before extracting to tableau
all_quarters_v3 %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


all_quarters_v4 %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


all_quarters_v5 %>%
  ggplot(aes(x = time_of_day, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


# Exporting csv
write.csv(all_quarters_v3, file = 'all_quarters_v3.csv')
write.csv(all_quarters_v4, file = 'all_quarters_v4.csv')
write.csv(all_quarters_v5, file = 'all_quarters_v5.csv')




