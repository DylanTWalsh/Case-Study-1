install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
install.packages("plyr")
install.packages("ggplot2")
install.packages("lubridate")

library(dplyr)
library(readr)
library("plyr")
library("ggplot2")
library(lubridate)
library(scales)

#========================
#Adding and cleaning data
#========================

#set the working directory 
setwd("C:/Users/plue9/Documents/Data Analysis/Case Study 1/CSV_files")

#confirm the working directory
getwd()

#merge all csv files into one data frame
df <- list.files(path="C:/Users/plue9/Documents/Data Analysis/Case Study 1/CSV_files", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows

#remove duplicated rows
df_nodupes <- df[!duplicated(df$ride_id), ]

#inspect columns for incongruities
str(df_nodupes)

#remove lat and long as they are not needed
df_nodupes <- df_nodupes %>%
  select(-c(start_lat, end_lat))

#split times apart in dates and times
df_nodupes$Start_Date <- as.Date(df_nodupes$started_at)
df_nodupes$Start_Time <- format(df_nodupes$started_at, "%H:%M:%S")
df_nodupes$End_Date <- as.Date(df_nodupes$started_at)
df_nodupes$End_Time <- format(df_nodupes$started_at, "%H:%M:%S")

#splits each date and time into its own column
df_nodupes$Start_Time_Y <- as.numeric(format(df_nodupes$started_at, "%Y"))
df_nodupes$Start_Time_Mo <- as.numeric(format(df_nodupes$started_at, "%m"))
df_nodupes$Start_Time_D <- as.numeric(format(df_nodupes$started_at, "%d"))
df_nodupes$Start_Time_H <- as.numeric(format(df_nodupes$started_at, "%H"))
df_nodupes$Start_Time_Mi <- as.numeric(format(df_nodupes$started_at, "%M"))
df_nodupes$Start_Time_S <- as.numeric(format(df_nodupes$started_at, "%S"))
df_nodupes$Start_Time_Y_M <- (format(df_nodupes$started_at, "%Y-%m"))

#finds specific day of the week this trip happened on
df_nodupes$Start_Time_weekday <- weekdays(as.Date(df_nodupes$started_at))

#get difference in end time and start time in minutes, researched from geeksforgeeks.org
df_nodupes <- df_nodupes %>%
  mutate(ride_in_minutes = difftime(df_nodupes$ended_at, df_nodupes$started_at, units = "mins"))

#converts ride time to numeric
is.factor(df_nodupes$ride_in_minutes)
df_nodupes$ride_in_minutes <- as.numeric(as.character(df_nodupes$ride_in_minutes))
is.numeric(df_nodupes$ride_in_minutes)

#changes data in member_casual column to be consistent where a paid member is member
#while non paid is casual
df_nodupes <- df_nodupes %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber"="member"
                                ,"Customer"="casual"))

#remove data where the ride_in_minutes is negative or 0
df_nodupes <- df_nodupes[!(df_nodupes$ride_in_minutes <=0),]

df_nodupes <- head(df_nodupes, -1)

#====================
#Descriptive Analysis
#====================

#analysis on ride length
mean(df_nodupes$ride_in_minutes)
median(df_nodupes$ride_in_minutes)
max(df_nodupes$ride_in_minutes)
min(df_nodupes$ride_in_minutes)

#analysis on ride length compared from casual and member users
aggregate(df_nodupes$ride_in_minutes~df_nodupes$member_casual, FUN = mean)
aggregate(df_nodupes$ride_in_minutes~df_nodupes$member_casual, FUN = median)
aggregate(df_nodupes$ride_in_minutes~df_nodupes$member_casual, FUN = max)
aggregate(df_nodupes$ride_in_minutes~df_nodupes$member_casual, FUN = min)

#sorts days of the week in order
df_nodupes$Start_Time_weekday <- ordered(df_nodupes$Start_Time_weekday, levels=c("Sunday","Monday",
                                                                                 "Tuesday","Wednesday","Thursday","Friday","Saturday"))

#find the average ride time of each day by each member type
aggregate(df_nodupes$ride_in_minutes~df_nodupes$member_casual + df_nodupes$Start_Time_weekday, FUN = mean)

#distribution between casual and members
df_nodupes %>%
  group_by(member_casual)%>%
  dplyr::summarise(number_of_rides = n(), average_duration = mean(ride_in_minutes))%>%
  arrange(member_casual)

#distribution between casual and members broken down by year/month
df_nodupes %>%
  group_by(member_casual, Start_Time_Y_M)%>%
  dplyr::summarise(number_of_rides = n(), average_duration = mean(ride_in_minutes))%>%
  arrange(member_casual, Start_Time_Y_M)

#Analyzes ridership data by type and weekday
df_nodupes %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday)%>%
  dplyr::summarise(number_of_rides = n(), average_duration = mean(ride_in_minutes))%>%
  arrange(member_casual, weekday)

#distribution between casual and members broken down by hour
df_nodupes %>%
  group_by(member_casual, Start_Time_H)%>%
  dplyr::summarise(number_of_rides = n(), average_duration = mean(ride_in_minutes))%>%
  arrange(member_casual, Start_Time_H)%>%
  print(n = 48)

#distribution between casual and members broken down by bike type
df_nodupes %>%
  group_by(member_casual, rideable_type)%>%
  dplyr::summarise(number_of_rides = n(), average_duration = mean(ride_in_minutes))%>%
  arrange(member_casual, rideable_type)

#distribution between casual and members broken down by bike type
df_nodupes %>%
  group_by(member_casual, rideable_type)%>%
  dplyr::summarise(number_of_rides = n(), average_duration = mean(ride_in_minutes))%>%
  arrange(member_casual, rideable_type)


df_nodupes %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday)%>%
  dplyr::summarise(number_of_rides = n(), average_duration = mean(ride_in_minutes))%>%
  arrange(member_casual, weekday)%>%
  ggplot(aes(x=weekday, y=number_of_rides, fill = member_casual))+
  geom_col(position="dodge")

df_nodupes %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday)%>%
  dplyr::summarise(number_of_rides = n(), average_duration = mean(ride_in_minutes))%>%
  arrange(member_casual, weekday)%>%
  ggplot(aes(x=weekday, y=average_duration, fill = member_casual))+
  geom_col(position="dodge")


#=======================================
#Export cleaned data for analysis to CSV
#=======================================

#export to csv file
write.csv(df_nodupes,"C:/Users/plue9/Documents/Data Analysis/Case Study 1/CSV_files/cleaned_csv_file.csv", row.names = FALSE)

#==============
#Analyzing data
#==============

df_nodupes %>%
  group_by(Start_Time_weekday) %>%
  summarize(mean = mean(ride_in_minutes))

#distribution between casual and members
ggplot(data = df_nodupes) +
  geom_bar(mapping = aes(member_casual, fill=member_casual)) +
  ggtitle("Distribution between casual and members")
  scale_y_continuous(labels = scales::comma)

#distribution between casual and members broken down by year/month
ggplot(data = df_nodupes) +
  geom_bar(mapping = aes(Start_Time_Y_M, fill=member_casual)) +
  ggtitle("Distribution between casual and members by year/month") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x=element_text(angle = 90))

#distribution between casual and members broken down by weekday
ggplot(data = df_nodupes) +
  geom_bar(mapping = aes(Start_Time_weekday, fill=member_casual)) +
  ggtitle("Distribution between casual and members by weekday") +
  scale_y_continuous(labels = scales::comma)

#distribution between casual and members broken down by hour
ggplot(data = df_nodupes) +
  geom_bar(mapping = aes(x=Start_Time_H, fill=member_casual)) +
  ggtitle("Distribution between casual and members by hour") +
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks=seq(23))

#distribution between casual and members broken down by hour
ggplot(data = df_nodupes) +
  geom_bar(mapping = aes(y=rideable_type, fill=member_casual)) +
  ggtitle("Distribution between casual and members by ride type") +
  scale_x_continuous(labels = scales::comma)

#distribution between casual and members broken down by bike type
ggplot(data = df_nodupes) +
  geom_bar(mapping = aes(y=rideable_type, fill=member_casual)) +
  ggtitle("Distribution between casual and members by ride type") +
  scale_x_continuous(labels = scales::comma)

ggplot(data = df_nodupes) +
  geom_line(mapping = aes(x=Start_Time_weekday,y=Start_Time_H))
 

#find total number of data of casual vs member. Need dplyr:: to get grouping
df_nodupes %>%
  group_by(member_casual) %>%
  dplyr::summarise(count = length(ride_id))

