library(tidyverse)
library(ggplot2)
library(lubridate)

daily_activity <- read.csv("dailyActivity_merged.csv")
daily_calories <- read.csv("dailyCalories_merged.csv")
daily_intensities <- read.csv("dailyIntensities_merged.csv")
daily_steps <- read.csv("dailySteps_merged.csv")
heartRate_secs <- read.csv("heartrate_seconds_merged.csv")
hourly_calories <- read.csv("hourlyCalories_merged.csv")
hourly_intensities <- read.csv("hourlyIntensities_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")
minute_calories <- read.csv("minuteCaloriesNarrow_merged.csv")
minute_intensities <- read.csv("minuteIntensitiesNarrow_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")
weight_info <- read.csv("weightLogInfo_merged.csv")

n_distinct(hourly_intensities$Id)
n_distinct(hourly_calories$Id)
n_distinct(hourly_steps$Id)

n_distinct(daily_intensities$Id)
n_distinct(daily_calories$Id)
n_distinct(daily_steps$Id)

n_distinct(sleep_day$Id)
n_distinct(weight_info$Id)






##  Clean & Analyze Sleep Activity ##

sleep_day

sleep_day$TotalHoursAsleep <- sleep_day$TotalMinutesAsleep/60
sleep_day
sleep_day$InBedButNotAsleep <- sleep_day$TotalTimeInBed-sleep_day$TotalMinutesAsleep
sleep_day

typeof(sleep_day$SleepDay)
sleep_day$SleepDay <- as.POSIXct(sleep_day$SleepDay,
                                 format= '%m/%d/%Y %H:%M:%S')
sleep_day

which(!complete.cases(sleep_day)) ## no NA values ##

uniq_comb <- sleep_day %>% select(Id, SleepDay,TotalHoursAsleep) %>%
  group_by(Id)

uniq_comb <- uniq_comb %>% group_by(Id) %>%
  summarise(n_of_counts=n_distinct(SleepDay),
            TotalHoursAsleep=TotalHoursAsleep)

uniq_comb <- uniq_comb %>% filter(n_of_counts > 20)

sleep_merged <- merge(uniq_comb,sleep_day,by="Id")
sleep_merged <- sleep_merged %>% select(Id,n_of_counts,TotalHoursAsleep.x,
                                        SleepDay,TotalMinutesAsleep,
                                        InBedButNotAsleep) %>%
  filter(n_of_counts > 20)

sleep_merged$Day <-  strftime(sleep_merged$SleepDay, format="%m-%d")
n_distinct(sleep_merged$Id)

## Minutes Asleep Dist ##

by_minutes <- sleep_merged %>% group_by(TotalMinutesAsleep) %>%
  summarise(Id=Id)
by_minutes

ggplot(by_minutes) +
  geom_histogram(aes(TotalMinutesAsleep),
                 binwidth = 20,
                 col="black") +
  labs(title="Distribution of Minutes Asleep")


## Hours Asleep Dist ##

by_hours <- sleep_merged %>% group_by(TotalHoursAsleep.x) %>%
  summarise(Id=Id)
by_hours

ggplot(by_hours) +
  geom_histogram(aes(TotalHoursAsleep.x),
                 binwidth = 1,
                 col="black") +
  labs(title="Distribution of Hours Asleep",
       x="Hours Asleep")


## In Bed Awake ##

in_bed_awake <- sleep_merged %>% group_by(InBedButNotAsleep) %>%
  summarise(Id=Id)
in_bed_awake

ggplot(in_bed_awake,aes(x=InBedButNotAsleep)) +
  geom_histogram(binwidth=10 ,col="black") +
  labs(title="Minutes in Bed But not Asleep",x="Minutes")





## Steps Analysis by the Hour (Correct Hour Timezone) ##

hourly_steps
typeof(hourly_steps$ActivityHour)

hourly_steps$ActivityHour <- mdy_hms(hourly_steps$ActivityHour, tz='EST')
hourly_steps

hourly_steps$Hour <- strftime(hourly_steps$ActivityHour, format="%H") 
hourly_steps

by_the_hour <- hourly_steps %>% group_by(Hour)
by_the_hour

ggplot(by_the_hour, aes(Hour,StepTotal)) +
  geom_bar(position = 'dodge',
           stat = 'summary',
           fun = 'mean') + 
  labs(title = "Average of Steps by the Hour")







## calories Analysis by the Hour ##

hourly_calories
hourly_calories$ActivityHour <- mdy_hms(hourly_calories$ActivityHour,tz='EST')

hourly_calories$HourOfDay <- strftime(hourly_calories$ActivityHour, format="%H") 
hourly_calories

uniq_comb2 <- hourly_calories %>% select(Id, ActivityHour,Calories,HourOfDay) %>%
  group_by(Id)

uniq_comb2 <- uniq_comb2 %>% group_by(Id) %>%
  summarise(n_of_counts=n_distinct(ActivityHour),
            Calories=Calories,
            HourOfDay=HourOfDay)

n_distinct(uniq_comb2$Id)

uniq_comb2 <- uniq_comb2 %>% group_by(Id) %>%
  summarise(n_of_counts=mean(n_of_counts))

hourly_cal_merged <- merge(hourly_calories,uniq_comb2,by="Id") %>%
  filter(n_of_counts > 300)

hourly_cal_merged ## Eliminate Id with less than 300 Observations ##

ggplot(cal_by_hour, aes(HourOfDay,Calories)) +
  geom_bar(position = 'dodge',
           stat = 'summary',
           fun = 'mean') + 
  labs(title = "Average of Calories Burned by the Hour")






## Calories Analysis by the Minute ##

minute_calories
minute_calories$ActivityMinute <- mdy_hms(minute_calories$ActivityMinute,tz='EST')

minute_calories$HourandMinute <- strftime(minute_calories$ActivityMinute, format="%H:%M") 
minute_calories

uniq_comb3 <- minute_calories %>% group_by(Id) %>%
  summarise(n_of_counts=n_distinct(ActivityMinute),
            Calories=Calories,
            HourandMinute=HourandMinute)
uniq_comb3 <- uniq_comb3 %>% group_by(Id) %>%
  summarise(n_of_counts=mean(n_of_counts)) %>%
  filter (n_of_counts > 30000) ## Filter by number of counts ##

minute_calories_filtered <- merge(minute_calories,uniq_comb3,by="Id")
minute_calories_filtered

minute_calories_filtered <- minute_calories_filtered %>%
  group_by(HourandMinute) %>%
  summarise(cal_avg=mean(Calories)) %>%
  arrange(desc(cal_avg))

top_cal_mins <- minute_calories_filtered[1:60,]
top_cal_mins

ggplot(top_cal_mins, aes(HourandMinute,cal_avg)) +
  geom_point() + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Average of Calories Burned by the Minute",
       y="Average of Calories Burned")





## Intensities by the Hour ##

hourly_intensities$ActivityHour <- mdy_hms(hourly_intensities$ActivityHour,tz='EST')

hourly_intensities$HourOfDay <- strftime(hourly_intensities$ActivityHour, format="%H") 
hourly_intensities

int_by_hour <- hourly_intensities %>% group_by(HourOfDay)

ggplot(int_by_hour, aes(HourOfDay,TotalIntensity)) +
  geom_bar(position = 'dodge',
           stat = 'summary',
           fun = 'mean') + 
  labs(title = "Average of Intensity by the Hour")






## Intensity by the minute ##

minute_intensities$ActivityMinute <- mdy_hms(minute_intensities$ActivityMinute,tz='EST')

minute_intensities$HourAndMinute <- strftime(minute_intensities$ActivityMinute, format="%H:%M")

intensity_byMins <- minute_intensities %>% select(HourAndMinute,Intensity) %>%
  group_by(HourAndMinute) %>%
  summarise(avg_intensity=mean(Intensity)) %>%
  arrange(desc(avg_intensity))

minute_intensities

uniq_comb4 <- minute_intensities %>% group_by(Id) %>%
  summarise(n_of_counts=n_distinct(ActivityMinute),
            Intensity=Intensity,
            HourAndMinute=HourAndMinute)
uniq_comb4 <- uniq_comb4 %>% group_by(Id) %>%
  summarise(n_of_counts=mean(n_of_counts)) %>%
  filter(n_of_counts > 30000) ## Filter Id by Observations ##


minute_intensities_filtered <- merge(minute_intensities,uniq_comb4,by="Id")
minute_intensities_filtered

minute_intensities_filtered <- minute_intensities_filtered %>%
  group_by(HourAndMinute) %>%
  summarise(intensity_avg=mean(Intensity)) %>%
  arrange(desc(intensity_avg))

top_intensity_mins <- minute_intensities_filtered[1:60,]
top_intensity_mins

ggplot(top_intensity_mins, aes(x=HourAndMinute,y=intensity_avg)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title="The 60 Mins With Highest Intensity",
       y="Average Intensity")






## Heartrate Analysis ##
heartRate_secs

heartRate_secs$Time <- mdy_hms(heartRate_secs$Time,tz='EST')
heartRate_secs$HourAndMinute <- strftime(heartRate_secs$Time, format="%H:%M") 
heartRate_secs

uniq_comb5 <- heartRate_secs %>% group_by(Id) %>%
  summarise(n_of_counts=n_distinct(Time)) ## Only 14 Users ##

heartRate_merged <- merge(heartRate_secs,uniq_comb5,by="Id") %>%
  filter(n_of_counts > 150000)

heartRate_merged

heart_by_min <- heartRate_merged %>%
  group_by(HourAndMinute) %>%
  summarise(avg_rate=mean(Value)) %>%
  arrange(desc(avg_rate))

top_heartRate_mins <- heart_by_min[1:60,]
top_heartRate_mins

ggplot(top_heartRate_mins, aes(x=HourAndMinute,y=avg_rate)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title="The 60 Mins with highest Heart Rate",
       y="Average Heart Rate")


heartRate_merged$Hour <- strftime(heartRate_merged$Time, format="%H")
heartRate_merged

heart_by_hour <- heartRate_merged %>% 
  group_by(Hour) %>%
  summarise(avg_rate=mean(Value))

ggplot(heart_by_hour, aes(x=Hour,y=avg_rate)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  labs(title="Average Heart Rate by the Hour",
       y="Average Heart Rate")  






## Analyze Variance within the top 4 heart rate hours ##

heartRate_at_12 <- filter(heartRate_merged, Hour == 12) %>%
  select(HourAndMinute, Value) %>%
  group_by(HourAndMinute) %>%
  summarise(avg_value=mean(Value))
  
ggplot(heartRate_at_12, aes(HourAndMinute,avg_value)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title="Average Heart Rate at 12 By the Minute")



heartRate_at_16 <- filter(heartRate_merged, Hour == 16) %>%
  select(HourAndMinute, Value) %>%
  group_by(HourAndMinute) %>%
  summarise(avg_value=mean(Value))

ggplot(heartRate_at_16, aes(HourAndMinute,avg_value)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title="Average Heart Rate at 16 By the Minute")



heartRate_at_17 <- filter(heartRate_merged, Hour == 17) %>%
  select(HourAndMinute, Value) %>%
  group_by(HourAndMinute) %>%
  summarise(avg_value=mean(Value))

ggplot(heartRate_at_17, aes(HourAndMinute,avg_value)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title="Average Heart Rate at 17 By the Minute")



heartRate_at_18 <- filter(heartRate_merged, Hour == 18) %>%
  select(HourAndMinute, Value) %>%
  group_by(HourAndMinute) %>%
  summarise(avg_value=mean(Value))

ggplot(heartRate_at_18, aes(HourAndMinute,avg_value)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title="Average Heart Rate at 18 By the Minute")







 ## Analysis of Calories ##

typeof(hourly_calories$ActivityHour)

hourly_calories$ActivityHour <- mdy_hms(hourly_calories$ActivityHour,tz='EST')

hourly_calories$HourAndMinute <- strftime(hourly_calories$ActivityHour, format="%H:%M") 

hourly_calories$Hour <- strftime(hourly_calories$ActivityHour, format="%H") 

avg_cal_byHour <- hourly_calories %>% select(Hour, Calories) %>%
  group_by(Hour) %>%
  summarise(avg_calories=mean(Calories))

ggplot(avg_cal_byHour, aes(x=Hour,y=avg_calories)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title="Average Calories Burned by the Hour ")






## Analysis of Steps ##

hourly_steps

hourly_steps$ActivityHour <- mdy_hms(hourly_steps$ActivityHour,tz='EST')

hourly_steps$Hour <- strftime(hourly_calories$ActivityHour, format="%H") 

avg_steps_byHour <- hourly_steps %>% select(Hour, StepTotal) %>%
  group_by(Hour) %>%
  summarise(avg_steps=mean(StepTotal))


ggplot(avg_steps_byHour, aes(x=Hour,y=avg_steps)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title="Average Steps by the Hour")





## Analysis of Daily Steps ##

daily_steps
daily_steps$ActivityDay <- mdy(daily_steps$ActivityDay, tz="EST")

daily_steps$Day <- strftime(daily_steps$ActivityDay, format="%m-%d")
n_distinct(daily_steps$Day)

steps_by_day <- daily_steps %>% select(Day,StepTotal) %>%
  group_by(Day) %>%
  summarise(steps_avg=mean(StepTotal)) 

ggplot(steps_by_day, aes(Day,steps_avg)) +
  geom_bar(position='dodge',
           stat='identity') +
  scale_x_discrete(guide=guide_axis(angle=45)) +
  labs(title="Daily Average of Steps")





## Analysis of Daily Intensities ##

daily_intensities

daily_intensities$TotalTrackedMinutes <- (daily_intensities$SedentaryMinutes +
                                          daily_intensities$LightlyActiveMinutes +
                                          daily_intensities$FairlyActiveMinutes +
                                          daily_intensities$VeryActiveMinutes)

daily_intensities$UntrackedMinutes <- (1440- daily_intensities$TotalTrackedMinutes)

daily_intensities$Sedentary_perc <- ((daily_intensities$SedentaryMinutes/daily_intensities$TotalTrackedMinutes)
                                     *100)
daily_intensities$LightlyActive_perc <- ((daily_intensities$LightlyActiveMinutes/daily_intensities$TotalTrackedMinutes)
                                     *100)
daily_intensities$FairlyActive_perc <- ((daily_intensities$FairlyActiveMinutes/daily_intensities$TotalTrackedMinutes)
                                     *100)
daily_intensities$VeryActive_perc <- ((daily_intensities$VeryActiveMinutes/daily_intensities$TotalTrackedMinutes)
                                     *100)
daily_intensities$TotalDistance <- (daily_intensities$SedentaryActiveDistance +
                                      daily_intensities$LightActiveDistance +
                                      daily_intensities$ModeratelyActiveDistance +
                                      daily_intensities$VeryActiveDistance)

daily_intensities$SedDist_perc <- ((daily_intensities$SedentaryActiveDistance/daily_intensities$TotalDistance)
                                     *100)
daily_intensities$LightDist_perc <- ((daily_intensities$LightActiveDistance/daily_intensities$TotalDistance)
                                         *100)
daily_intensities$ModeDist_perc <- ((daily_intensities$ModeratelyActiveDistance/daily_intensities$TotalDistance)
                                        *100)
daily_intensities$ActiveDist_perc <- ((daily_intensities$VeryActiveDistance/daily_intensities$TotalDistance)
                                      *100)

daily_intensities$ActivityDay <- mdy(daily_intensities$ActivityDay,tz='EST')
daily_intensities$Day <- strftime(daily_intensities$ActivityDay, format="%m-%d")
daily_intensities




## Analysis of Daily Calories ##

daily_calories

daily_calories$ActivityDay <- mdy(daily_calories$ActivityDay, tz="EST")
daily_calories$Day <- strftime(daily_calories$ActivityDay, format="%m-%d")

calories_by_day <- daily_calories %>% select(Day,Calories) %>%
  group_by(Day) %>%
  summarise(calories_avg=mean(Calories))

ggplot(calories_by_day, aes(Day,calories_avg)) +
  geom_bar(position='dodge',
           stat='identity') +
  scale_x_discrete(guide=guide_axis(angle=45)) +
  labs(title="Daily Average of Calories")




## Analysis of Daily Activity ##

daily_activity

daily_activity$ActivityDate <- mdy(daily_activity$ActivityDate,tz="EST")
daily_activity$Day <- strftime(daily_activity$ActivityDate, format="%m-%d")

uniq_comb6 <- daily_activity %>% group_by(Id) %>%
  summarise(n_of_counts=n_distinct(Day))

daily_activity_merged <- merge(daily_activity, uniq_comb6, by="Id") %>%
  filter(n_of_counts > 20)

daily_activity_merged


## Steps Tracked across time ##

day_to_steps <- daily_activity_merged %>% group_by(Day) %>%
  summarise(avg_steps=mean(TotalSteps))
  
day_to_steps$Day <- as.Date(day_to_steps$Day, format='%m-%d')
day_to_steps$avg_steps <- as.integer(day_to_steps$avg_steps)

ggplot(day_to_steps, aes(Day,avg_steps)) +
  geom_smooth(method="lm") +
  labs(title="Daily Average of Steps",
         y="Average of steps")

## Distance Tracked Across time ##

day_to_activeDist <- daily_activity_merged %>%
  group_by(Day) %>%
  summarise(avg_activeDist=mean(VeryActiveDistance))

day_to_activeDist$Day <- as.Date(day_to_activeDist$Day, format='%m-%d')
day_to_activeDist$avg_activeDist <- as.integer(day_to_activeDist$avg_activeDist)

ggplot(day_to_activeDist,aes(Day,avg_activeDist)) +
  geom_smooth(method="lm") +
  labs(title="Daily Average of Active Distance",
       y="Average of Active Distance (km)")


## Calories Tracked Across time ##

day_to_calories <- daily_activity_merged %>%
  group_by(Day) %>%
  summarise(avg_calories=mean(Calories))

day_to_calories$Day <- as.Date(day_to_calories$Day, format='%m-%d')
day_to_calories$avg_calories <- as.integer(day_to_calories$avg_calories)

ggplot(day_to_calories,aes(Day,avg_calories)) +
  geom_smooth(method="lm") +
  labs(title="Daily Average of Calories Burned",
       y="Average of Calories Burned")

