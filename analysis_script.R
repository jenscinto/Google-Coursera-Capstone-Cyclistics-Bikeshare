## Capstone Study 1 - Cyclistic Bikeshare
## Author: Jen Scinto
## Date: 10-14-24

## This script contains the analysis I completed for the capstone project for
## the Google/Coursera Data Analytics Certificate course. This analysis uses 12
## months of Divvy bikeshare data, with personal indentifying information
## removed, to provide business insights to Cyclistics, a fictional company,
## about ways that they could convert casual users to annual memberships.

## initiating libraries
install.packages('tidyverse')
library(tidyverse)
install.packages('dplyr')
library(dplyr)
install.packages('geosphere')
library(geosphere)
install.packages('lubridate')
library(lubridate)
install.packages('hms')
library(hms)
install.packages('scales')
library(scales)


## Import raw data from .csv files
data_1023 <- read.csv("202310-divvy-tripdata.csv")
data_1123 <- read.csv("202311-divvy-tripdata.csv")
data_1223 <- read.csv("202312-divvy-tripdata.csv")
data_0124 <- read.csv("202401-divvy-tripdata.csv")
data_0224 <- read.csv("202402-divvy-tripdata.csv")
data_0324 <- read.csv("202403-divvy-tripdata.csv")
data_0424 <- read.csv("202404-divvy-tripdata.csv")
data_0524 <- read.csv("202405-divvy-tripdata.csv")
data_0624 <- read.csv("202406-divvy-tripdata.csv")
data_0724 <- read.csv("202407-divvy-tripdata.csv")
data_0824 <- read.csv("202408-divvy-tripdata.csv")
data_0924 <- read.csv("202409-divvy-tripdata.csv")

##Combine individual months into one data frame with 12 months of data
trip_data <- bind_rows(data_1023, data_1123, data_1223, data_0124, data_0224, data_0324, data_0424, data_0524, data_0624, data_0724, data_0824, data_0924)

## Remove original placeholder import data frames
rm(data_1023, data_1123, data_1223, data_0124, data_0224, data_0324, data_0424, data_0524, data_0624, data_0724, data_0824, data_0924)


## Create new columns to store date info (trip start and end as datetime format, trip month and trip year)
trip_data <- trip_data %>%
  mutate(trip_start_date = as_datetime(started_at),
         trip_end_date = as_datetime(ended_at),
         trip_month = format(trip_start_date,"%B"),
         trip_year = format(trip_start_date, "%Y"))

## Concatenate the month and year of the trip so the axis labels make more sense
trip_data <- trip_data %>%
  mutate(trip_month_year = paste(trip_month, trip_year, sep = " "))

## Create a vector of month & year labels so the data can be graphed in chronological order rather than alphabetical (learned to do this from: https://guslipkin.medium.com/reordering-bar-and-column-charts-with-ggplot2-in-r-435fad1c643e)
study_months <- c("October 2023", "November 2023", "December 2023", "January 2024", "February 2024", "March 2024", "April 2024", "May 2024", "June 2024", "July 2024", "August 2024", "September 2024")


## Investigation 1 - Plot ride data by user type by month
ggplot(data = trip_data) + 
  geom_bar(mapping = aes(x = factor(trip_month_year, study_months), fill=member_casual), position="dodge") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_y_continuous(labels = comma) +
  labs(title = "Number of Rides per Month",
       x = "Month",
       y = "Number of Rides",
       fill = "Membership Type")

## Investigation 2 - Average distance of trip

## Calculate the distance as the crow flies between the start lat/long and end lat/long
trip_data <- trip_data %>%
  mutate(start_coord = cbind(start_lng, start_lat),
         end_coord = cbind(end_lng, end_lat),
         distance_crow = distHaversine(start_coord, end_coord, r=3961))

## Calculate the average distance_crow for members vs. casual users
trip_data %>%
  filter(member_casual == "member" & distance_crow < 10) %>%
  summarise(mean(distance_crow, na.rm = TRUE)) -> avg_trip_dist_members

trip_data %>%
  filter(member_casual == "casual" & distance_crow < 10) %>%
  summarise(mean(distance_crow,na.rm=TRUE)) -> avg_trip_dist_casual

#Filter data to 99.9% distances to remove outliers (99.9 %ile is 9.03 miles ) and then plot as histogram
trip_data %>%
  filter(distance_crow < 10) %>%
  ggplot(aes(x=distance_crow, fill=member_casual)) +
  geom_histogram(bins = 20, boundary = 0, color = "black") +
  facet_wrap(~member_casual)+
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(0:10)) +
  scale_fill_manual(values=c("#F8766D","#00BFC4")) +
  labs(title = "Number of Rides by Trip Distance",
       x = "Trip Distance (as the crow flies) (miles)",
       y = "Number of Rides",
       fill = "Membership Type")


## Investigation 3 - Average length of trip in minutes

trip_data <- trip_data %>%
  mutate(trip_length_time = as.numeric(difftime(started_at, ended_at, units="mins")))

trip_data %>%
  filter(abs(trip_length_time) < 105) %>%
  ggplot(aes(x=abs(trip_length_time), fill = member_casual)) +
  geom_histogram(binwidth = 5, boundary = 0, color = "black") +
  facet_wrap(~member_casual) +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=c("#F8766D","#00BFC4")) +
  labs(title = "Number of Rides by Trip Length (Time)",
       x = "Trip Length (minutes)",
       y = "Number of Rides",
       fill = "Membership Type")



# Investigation 4 - Day of the week for rides
trip_data <- trip_data %>%
  mutate(day_of_week = weekdays(trip_start_date))

days_of_the_week = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

ggplot(data=trip_data) +
  geom_bar(mapping = aes(x=factor(day_of_week, days_of_the_week), fill=member_casual), position="dodge")+
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  labs(title = "Number of Rides by Day of the Week",
       x = "Day of the Week",
       y = "Number of Rides",
       fill = "Membership Type")

## Investigation 5 - Type of rideable used
ggplot(data=trip_data) +
  geom_bar(mapping = aes(x=rideable_type, fill=member_casual), position="dodge")+
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("Classic Bike", "Electric Bike", "Scooter"))+
  labs(title = "Rideable Selection by User Type",
       x = "Type of Rideable",
       y = "Number of Rides",
       fill = "Membership Type")

## Investigation 6 - Overall number of rides by member type
ggplot(data=trip_data)+
  geom_bar(mapping = aes(x=member_casual, fill=member_casual), width =0.5) +
  scale_y_continuous(labels=comma)+
  labs(title = "Total Rides by User Type",
       subtitle = "For Rides October 2023 through September 2024",
       x = "User Type",
       y = "Number of Rides",
       fill="Membership Type")

