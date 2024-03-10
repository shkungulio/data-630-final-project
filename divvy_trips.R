#Professor: Elizabeth Cudney Ph.D.
#Student: Seif Kungulio
#Course: DATA 630
#Project: 2023 Divvy Tripdata Analysis Project


#===============================================================================
# PART ONE                                              
#===============================================================================
# The purpose of this project is to consolidate downloaded Divvy data into a 
# single data frame and then conduct simple analysis to help answer the key
# question: "In what ways do members and casual riders use Divvy bikes
# differently?"


#===============================================================================
# INSTALL REQUIRED LIBRARIES
#===============================================================================
library(tidyverse) # for data import and wrangling
library(lubridate) # helps wrangle date attributes
library(ggplot2) # helps with visualization


#===============================================================================
# STEP 1: DATA COLLECTION
#===============================================================================
# Upload the divvy data sets (csv files)
jan_23 <- read_csv("sources/202301-divvy-tripdata.csv", show_col_types = FALSE)
feb_23 <- read_csv("sources/202302-divvy-tripdata.csv", show_col_types = FALSE)
mar_23 <- read_csv("sources/202303-divvy-tripdata.csv", show_col_types = FALSE)
apr_23 <- read_csv("sources/202304-divvy-tripdata.csv", show_col_types = FALSE)
may_23 <- read_csv("sources/202305-divvy-tripdata.csv", show_col_types = FALSE)
jun_23 <- read_csv("sources/202306-divvy-tripdata.csv", show_col_types = FALSE)
jul_23 <- read_csv("sources/202307-divvy-tripdata.csv", show_col_types = FALSE)
aug_23 <- read_csv("sources/202308-divvy-tripdata.csv", show_col_types = FALSE)
sep_23 <- read_csv("sources/202309-divvy-tripdata.csv", show_col_types = FALSE)
oct_23 <- read_csv("sources/202310-divvy-tripdata.csv", show_col_types = FALSE)
nov_23 <- read_csv("sources/202311-divvy-tripdata.csv", show_col_types = FALSE)
dec_23 <- read_csv("sources/202312-divvy-tripdata.csv", show_col_types = FALSE)


#===============================================================================
# STEP 2: DATA WRANGLING AND FILES CONSOLIDATION
#===============================================================================
# Compare the column name for each file.
# While the names do not have to be in the same order, they DO need to match
# perfectly before joining them into single file
colnames(jan_23)
colnames(feb_23)
colnames(mar_23)
colnames(apr_23)
colnames(may_23)
colnames(jun_23)
colnames(jul_23)
colnames(aug_23)
colnames(sep_23)
colnames(oct_23)
colnames(nov_23)
colnames(dec_23)

# Inspect the data frame and look for inconsistencies
str(jan_23)
str(feb_23)
str(mar_23)
str(apr_23)
str(may_23)
str(jun_23)
str(jul_23)
str(aug_23)
str(sep_23)
str(oct_23)
str(nov_23)
str(dec_23)

# Combine individual data frame files into single data frame file
all_trips <- bind_rows(jan_23, feb_23, mar_23, apr_23, may_23, jun_23,
                       jul_23, aug_23, sep_23, oct_23, nov_23, dec_23)

# Inspect the structure  and summary of the combined file
str(all_trips)
summary(all_trips)


#===============================================================================
# STEP 3: DATA CLEANING
#===============================================================================
# Remove unnecessary columns
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, start_station_name,
            start_station_id, end_station_name, end_station_id))

# Check and remove all rows where the "started_at" is greater than "ended_at"
all_trips <- all_trips %>%
  filter(all_trips$started_at < all_trips$ended_at)

# Inspect the data frame
colnames(all_trips) # check the column names
dim(all_trips) # check the dimension of the data set
str(all_trips) # check the structure of the data set
summary(all_trips) # get the overall summary of the data set
head(all_trips) 
tail(all_trips) 

# Generate frequency tables of ridable_type and member_casual
table(all_trips$rideable_type)
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month and day.
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")

# Add "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

# Rearrange the day_of_week
all_trips$day_of_week <- ordered(all_trips$day_of_week,
                                 levels=c("Sunday","Monday","Tuesday","Wednesday",
                                          "Thursday","Friday","Saturday"))

# Inspect the structure of the data set
str(all_trips)

# Convert "ride_length" to numeric so that we can perform calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


#===============================================================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#===============================================================================
# Descriptive analysis on ride_length (all figures are on seconds)
summary(all_trips$ride_length)

# Compare members and casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN=mean)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN=median)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN=max)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN=min)

# Check the average ride time by each day for member vs casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual + 
            all_trips$day_of_week, FUN=mean)

# Analyze ridership data by type and weekday
all_trips %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)

# Visualize the number of rides by rider type
all_trips %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = 'dodge')

# Visualize the average duration
all_trips %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual)) +
  geom_col(position = 'dodge')


#===============================================================================
# STEP 5: EXPORT THE FILE FOR FURTHER ANALYSIS
#===============================================================================
# Create a csv file that can be used for visualization in other platforms like
# Tableau, Power BI, Python(Bokeh), or R(Shiny)

write.csv(all_trips, file='./visuals/divy_trips.csv')

