# Cyclistic_Case_Study

# Introduction and Scenerio 
*The following case study is based on a fictitious company we will call Cyclistic, this analysis process was performed for the Google Data Analytics Certificate program on Coursera*

I am a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago.In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geo-tracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime.

The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, my team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, my team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve these recommendations, so they must be backed up with compelling data insights and professional data visualizations.

### Phase 1: Ask
The director of marketing has assigned me with the task of answering the question **How do annual members and casual riders use Cyclistic bikes differently?**

* The goal of this case study is to determine how annual members and casual riders use the Cyclistic bikes differently, and how can those differences inform the marketing team to create digital marketing materials to transform more casual riders into annual members.

* Primary stakeholders: The director of marketing and Cyclistic executive team

* Secondary stakeholders: Cyclistic marketing analytics team


### Phase 2: Prepare Data

The data that we will be using is Cyclistic’s historical trip data from last 12 months (May-2020 to Apr-2021). The data has been made available by Motivate International Inc. on this [link](https://divvy-tripdata.s3.amazonaws.com/index.html) under [this license](https://ride.divvybikes.com/data-license-agreement)

The dataset pulled for this analysis consists of 12 CSV files with 13 columns and more than 4 million rows.

ROCCC approach is used to determine the credibility of the data

* Reliable – It is complete and accurate and it represents all bike rides taken in the city of Chicago for the selected duration of our analysis.
* Original - The data is made available by Motivate International Inc. which operates the city of Chicago’s Divvy bicycle sharing service which is powered by Lyft.
* Comprehensive - the data includes all information about ride details including starting time, ending time, station name, station ID, type of membership and many more.
* Current – It is up-to-date as it includes data until end of May 2021
* Cited - The data is cited and is available under Data License Agreement.

Unfortunately, the ride_id is not particularly useful in this data set as it does not give each rider a reusable ride_id that follows them through every transaction or even through an entire day pass, the ride_id is new for each individual bike ride. With this missing information, we cannot attribute the casual riders to individuals and do not know how many times they have bought a single ride pass or how many rides they used with a day pass.

Without this individualized information, it will be difficult to conclude how many casual users have continued to use the service overtime or how often the annual users are using this service.This missing information will limit this analysis from being able to create marketing materials for users who have used the service several times, or to give more information on how often casual vs. members use the bike service over time. 

### Phase 3: Processing Data (Cleaning and Manipulating)

I downloaded each .csv individually and opened them in Google Sheets to inspect that the columns were the same in each file before importing those files into R Studio. Using R, I joined all 12 files into a single data frame in order to make comparisons throughout the entire year's time, after loading in the necessary package: tidyverse. 



```{r} 
library(tidyverse)

library(readr)
june22 <- read_csv("Capstone_Project/cyclistic_12monthdata/202206-divvy-tripdata.csv")
july22 <- read_csv("Capstone_Project/cyclistic_12monthdata/202207-divvy-tripdata.csv")
aug22 <- read_csv("Capstone_Project/cyclistic_12monthdata/202208-divvy-tripdata.csv")
sep22 <- read_csv("Capstone_Project/cyclistic_12monthdata/202209-divvy-publictripdata.csv")
oct22 <- read_csv("Capstone_Project/cyclistic_12monthdata/202210-divvy-tripdata.csv")
nov22 <- read_csv("Capstone_Project/cyclistic_12monthdata/202211-divvy-tripdata.csv")
dec22 <- read_csv("Capstone_Project/cyclistic_12monthdata/202212-divvy-tripdata.csv")
jan23 <- read_csv("Capstone_Project/cyclistic_12monthdata/202301-divvy-tripdata.csv")
feb23 <- read_csv("Capstone_Project/cyclistic_12monthdata/202302-divvy-tripdata.csv")
mar23 <- read_csv("Capstone_Project/cyclistic_12monthdata/202303-divvy-tripdata.csv")
apr23 <- read_csv("Capstone_Project/cyclistic_12monthdata/202304-divvy-tripdata.csv")
may23 <- read_csv("Capstone_Project/cyclistic_12monthdata/202305-divvy-tripdata.csv")

all_tripdata <- bind_rows(may23, apr23, mar23, feb23, jan23, dec22, nov22, oct22, sep22, aug22, july22, june22)
```


Once all of the necessary data has been imported and combined into a single data frame, it is time to start to clean and sort this data into more usable and clear formats. In the code noted below, you will see the steps taken to insure that the data is clean, sorted and ready to analyze. I used R studio to clean and manipulate this data.

```{r}
# identifying missing values in the data
print(data.frame(sapply(all_tripdata, function(x) sum(length(which(is.na(x)))))))

```
As you can see above, the only columns that have null values are for the station ids, names, lat and long for both the start and end rides. This information that is missing isn't pertinent for this analysis, and the columns needed are free from null values. 

Next, I added a column days_of_the_week in weekday names format, and a ride_length column to give the length of each ride in minutes. I also removed any rows where the ride length was less than 0 minutes, as this should be impossible and is not valid for analysis.

```{r}
# create columns for day of the week and ride length
all_tripdata$date <- as.Date(all_tripdata$started_at)
  all_tripdata$day_of_week <- weekdays(all_tripdata$date)

all_tripdata$ride_length <- difftime(all_tripdata$ended_at, all_tripdata$started_at)
all_tripdata$ride_length <- all_tripdata$ride_length/60

# Converting "ride_length" to numeric so that I can run calculations on the data
all_tripdata$ride_length <- as.numeric(as.character(all_tripdata$ride_length))
all_tripdata$ride_length <- round(all_tripdata$ride_length, 0)

#Remove all rows with ride lengths less than 0
all_tripdata <- filter(all_tripdata, ride_length > 0)
```

### Phase 4: Analyze Data

Using R studio, I ran the following code to find  comparisons of casual riders vs. Cyclistic members. 

```{r}
# Descriptive analysis on ride_length (all figures in minutes)
mean(all_tripdata$ride_length) #straight average (total ride length / rides)
median(all_tripdata$ride_length) #midpoint number in the ascending array of ride lengths
max(all_tripdata$ride_length) #longest ride
min(all_tripdata$ride_length) #shortest ride

summary(all_tripdata$ride_length)

# Compare members and casual users
aggregate(all_tripdata$ride_length ~ all_tripdata$member_casual, FUN = mean)
aggregate(all_tripdata$ride_length ~ all_tripdata$member_casual, FUN = median)
aggregate(all_tripdata$ride_length ~ all_tripdata$member_casual, FUN = max)
aggregate(all_tripdata$ride_length ~ all_tripdata$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_tripdata$ride_length ~ all_tripdata$member_casual + all_tripdata$day_of_week, FUN = mean)


# Put the days of the week in order
all_tripdata$day_of_week <- ordered(all_tripdata$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_tripdata$ride_length ~ all_tripdata$member_casual + all_tripdata$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_tripdata %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
  ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

#Export analysis data to Excel to create visualizations

#Average ride length per day by rider type
counts <- aggregate(all_tripdata$ride_length ~ all_tripdata$member_casual + all_tripdata$day_of_week + all_tripdata$, FUN = mean)
write_csv(counts, "C:/Users/esg50/OneDrive/Desktop/counts.csv")

# Total and Average number of weekly rides by rider type
summary_wd <- all_tripdata %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%    
  arrange(member_casual, weekday)
write_csv(summary_wd, "C:/Users/esg50/OneDrive/Desktop/Capstone_Project/summary_wd.csv")

# Total and Average number of monthly rides by rider type
summary_month <- all_tripdata %>% 
  mutate(month = month(started_at, label = TRUE)) %>%  
  group_by(month,member_casual) %>%  
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%    
  arrange(month, member_casual)
write_csv(summary_month,"C:/Users/esg50/OneDrive/Desktop/Capstone_Project/summary_month.csv")

# Stations most used by each user group
summary_station <- all_tripdata %>% 
  mutate(station = start_station_name) %>%
  drop_na(start_station_name) %>% 
  group_by(start_station_name, member_casual) %>%  
  summarise(number_of_rides = n()) %>%    
  arrange(number_of_rides)
write_csv(summary_station, "C:/Users/esg50/OneDrive/Desktop/Capstone_Project/summary_station.csv")
```

### Phase 5: Share
To view the presentation based on this data, you can view my Slides Presentation [here](https://docs.google.com/presentation/d/1qreP9ar2FyEQEcXniZqNnct1XbZAIfWVJLjCIlOUUNI/edit?usp=sharing).  

### Phase 6: Act
To view my professional portfolio which has this entire case study available and recommendations click [here](https://github.com/emrussell21/Cyclistic_Case_Study/upload/main)
