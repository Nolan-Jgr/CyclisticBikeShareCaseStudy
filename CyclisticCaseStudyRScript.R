# Cyclistic Bike Share Case Study
# Created by: Nolan Jaeger
# Date Created: 01/07/2026
# Last Updated: 01/10/2026
install.packages('tidyverse')
library(tidyverse)
library(conflicted)
conflict_prefer("filter","dplyr")
conflict_prefer("lag","dplyr")
# Each dataset is saved to a dataframe
q1_2019 <- read_csv("Divvy_Trips_2019_Q1 - Sheet1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1 - Sheet1.csv")

# Column names from 2019 is renamed to match the corresponding
# Column names found in 2020
q1_2019 <- rename(q1_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype
                  )
# Data types for ride id and rideable type in 2019 are changed to match
# The data types of the corresponding attributes in 2020
q1_2019 <- mutate(q1_2019,
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
# Both the 2019 and 2020 dataframes are stacked and saved in a new dataframe
all_trips <- bind_rows(q1_2019,q1_2020)
# Lat,Long,Birthyear, and Gender attributes are removed
all_trips <- all_trips %>%
  select(-c(start_lat,start_lng,end_lat,end_lng,birthyear,gender,"tripduration"))

# The customer type field is consolidated.
# All instances of "Subscriber" are changed to "member"
# All instances of "Customer" are changed to "casual"
all_trips <- all_trips %>%
  mutate(member_casual = gsub("Subscriber", "member",member_casual))
all_trips <- all_trips %>%
  mutate(member_casual = gsub("Customer", "casual",member_casual))

# Added additional fields for date, month, day, and year 
all_trips$date <- as.Date(all_trips$started_at, format = "%m/%d/%Y")
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")

# All negative ride lengths are removed from the data and saved as a new dataframe
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length_seconds<0),]

# Mean Ride Length
x <- mean(all_trips_v2$ride_length_seconds)
# Midpoint Ride Length
mid <- median(all_trips_v2$ride_length_seconds)
# Maximum Ride Length
top <- max(all_trips_v2$ride_length_seconds)
# Minimum Ride Length
bottom <- min(all_trips_v2$ride_length_seconds)
# Summary
summary(all_trips_v2$ride_length_seconds)

# Comparison between member and casual users
aggregate(all_trips_v2$ride_length_seconds ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length_seconds ~ all_trips_v2$member_casual, FUN = median)
aggregate(ride_length_seconds ~ member_casual,
          data = all_trips_v2,
          FUN = max)
aggregate(ride_length_seconds ~ member_casual,
          data = all_trips_v2,
          FUN = min)
# Mean Ride Time by each day for each user type
aggregate(ride_length_seconds ~ member_casual + day_of_week,
          data = all_trips_v2,
          FUN = mean)
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,
                                    levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# Calculations for Ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(date, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),		 
            average_duration = mean(ride_length_seconds)) %>%
  arrange(member_casual, weekday)	

#Visualizations
# Number of Rides for each type by day of the week
all_trips_v2 %>%
  mutate(weekday = wday(date, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length_seconds)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x=weekday,y=number_of_rides,fill = member_casual)) +
  geom_col(position = "dodge")
# Average Duration of rides by each type
all_trips_v2 %>% 
  mutate(weekday = wday(date, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_seconds)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

