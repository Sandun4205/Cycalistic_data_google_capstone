#Load the tidyverse, lubridate, ggplot2, sqldf and psych libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
install.packages("plotrix")
library(plotrix)

#import the 'csv' files
df_202108<-read_csv("202108-divvy-tripdata.csv")
df_202109<-read_csv("202109-divvy-tripdata.csv")
df_202110<-read_csv("202110-divvy-tripdata.csv")
df_202111<-read_csv("202111-divvy-tripdata.csv")
df_202112<-read_csv("202112-divvy-tripdata.csv")
df_202201<-read_csv("202201-divvy-tripdata.csv")
df_202202<-read_csv("202202-divvy-tripdata.csv")
df_202203<-read_csv("202203-divvy-tripdata.csv")
df_202204<-read_csv("202204-divvy-tripdata.csv")
df_202205<-read_csv("202205-divvy-tripdata.csv")
df_202206<-read_csv("202206-divvy-tripdata.csv")
df_202207<-read_csv("202207-divvy-tripdata.csv")

#checking the struchture of the data sets
str(df_202108)
str(df_202109)
str(df_202110)
str(df_202111)
str(df_202112)
str(df_202201)
str(df_202202)
str(df_202203)
str(df_202204)
str(df_202205)
str(df_202206)
str(df_202207)

# merge individual data frames together for analysing
cyclistic_data <- rbind(df_202108,df_202109,df_202110, df_202111,df_202112,df_202201,df_202202, 
df_202203, df_202204, df_202205,df_202206,df_202207)

#check the structure of the new data frame and take a glimps
str(cyclistic_data)

glimpse(cyclistic_data)

nrow(cyclistic_data)

#removing column
#cyclistic_data <- subset(cyclistic_data, select = -c(month,year_month_))

#Add column for year-month
cyclistic_data$date <- as.Date(cyclistic_data$started_at)
cyclistic_data$month <- format(as.Date(cyclistic_data$date), '%B')

#add the day_of_week column
cyclistic_data$day_of_week  <- weekdays(cyclistic_data$started_at)

#Checking the member_casual and rideable_type column for any inconsistencies 
unique(cyclistic_data$rideable_type)
unique(cyclistic_data$member_casual)

#removing columns with NA values
cyclistic_data_cleaned <- cyclistic_data[,colSums(is.na(cyclistic_data))==0] 

#unique(cyclistic_data$start_station_name[grep("TEST",cyclistic_data$start_station_name)])

#Calculate the ride length in minutes
cyclistic_data_cleaned$ride_length <- difftime(cyclistic_data_cleaned$ended_at, cyclistic_data_cleaned$started_at, units = "mins")
cyclistic_data_cleaned$ride_length <- round(cyclistic_data_cleaned$ride_length,2)

#convert ride_length values to numerical
cyclistic_data_cleaned$ride_length <- as.numeric(cyclistic_data_cleaned$ride_length)

#removing observation where ride length is equal or less than 0
cyclistic_data_cleaned <- filter(cyclistic_data_cleaned, ride_length > 0)

#all the data cleaning done, Now ready to make some calculations on the data set to use in the analysis

# summary of the members and casual users
cyclistic_data_cleaned %>%
  group_by(member_casual) %>% 
  summarise(avg_ride_lng = mean(ride_length), median_ride_lng = median(ride_length), min_ride_lng = min(ride_length), max_ride_lng = max(ride_length))

#number_of_rides <- NROW(cyclistic_data_cleaned)

#make the order in day_of_week and month columns
cyclistic_data_cleaned$day_of_week <- ordered(cyclistic_data_cleaned$day_of_week, levels=c("Monday","Tuesday","Wednesday", "Thursday", 
                                                                                           "Friday", "Saturday", "Sunday"))
cyclistic_data_cleaned$month <- ordered(cyclistic_data_cleaned$month, levels=c("January","February","March", "April", 
                                                                               "May", "June", "July","August","September","October","November","December"))
#graphing the rides in a week 
options(scipen=10000)
cyclistic_data_cleaned %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), avarage_duration = mean(ride_length))%>%
  ggplot(aes(x=day_of_week, y=number_of_rides, fill = member_casual )) +
  geom_col(position = "dodge") + labs (title = "Total number of rides by day", x= "Week Day", Y ="Number of Rides") +
  theme(axis.text.x  = element_text(angle = 60, hjust = 1) )
class(cyclistic_data_cleaned$year_month_)

#calculating the precentage of casual riders on Weekdays and Weekend
casual_rides_Wdays <- NROW(filter(cyclistic_data_cleaned, member_casual == "casual" & !(day_of_week == "Saturday" | day_of_week == "Sunday")))
casual_rides_Wdays
casual_rides_Wends <- NROW(filter(cyclistic_data_cleaned, member_casual == "casual" & (day_of_week == "Saturday" | day_of_week == "Sunday")))
casual_rides_Wends

V1 <- c(casual_rides_Wdays,casual_rides_Wends)
presentage_of <- round(V1/sum(V1)*100,1)
lables <- c("Mon-Fri", "Sat-Sun" )
rep1 <- paste(lables, presentage_of)
rep1 <-paste(rep1, "%", sep = " ")
rep1  

#Analysing total rides by month
cyclistic_data_cleaned %>% 
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), avg_duration = mean(ride_length)) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x=month, y=number_of_rides, fill= member_casual)) +
  geom_col(position = 'dodge')+
  labs(title = 'total number of rides by month', x= 'Month', Y= 'Number of Rides') + theme(axis.text.x = element_text(angle =60, hjust = 1))

  #geom_line(aes(x = month, y = number_of_rides), size = 1.5, color="red", group = 1)






             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
