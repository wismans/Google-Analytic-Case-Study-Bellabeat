install.packages("tidyverse")
install.packages("janitor")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("skimr")
install.packages("lubridate")
install.packages("convertr")

library(tidyverse)
library(janitor)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(skimr)
library(lubridate)
library(convertr)

daily_activity <- read.csv("/cloud/project/Sandra/dailyActivity_merged.csv")
daily_sleep <- read.csv("/cloud/project/Sandra/sleepDay_merged.csv")
weight_log <- read.csv("/cloud/project/Sandra/weightLogInfo_merged.csv")

head(daily_activity)
head(daily_sleep)
head(weight_log)

colnames(daily_activity)
colnames(daily_sleep)
colnames(weight_log)

daily_activities <- clean_names(daily_activity)
daily_sleep_log <- clean_names(daily_sleep)
weights_log <- clean_names(weight_log)

colnames(daily_activities)
colnames(daily_sleep_log)
colnames(weights_log)

head(daily_activities)
head(daily_sleep_log)
head(weights_log)

glimpse(daily_activities)
glimpse(daily_sleep_log)
glimpse(weights_log)

sleep_new <- daily_sleep_log %>% 
  mutate(new_id = as.character(id)) %>% 
  mutate(date = as.Date(sleep_day, "%m/%d/%Y %H:%M:%S")) %>% 
  select(new_id, date, total_sleep_records, total_minutes_asleep, total_time_in_bed)
  
weight_new <- weights_log %>% 
  mutate(new_id = as.character(id)) %>% 
  mutate(new_date = as.Date(date, "%m/%d/%Y %H:%M:%S")) %>% 
  select(new_id, new_date, weight_kg) %>% 
  rename(date = new_date) %>% 
  rename(wt_entry = weight_kg)

daily_new <- daily_activities %>% 
  mutate(new_id = as.character(id)) %>% 
  mutate(date = as.Date(activity_date, "%m/%d/%Y")) %>% 
  select(-id, -activity_date)

participation <- tibble(weight = n_distinct(weight_new$new_id), activity = n_distinct(daily_new$new_id), sleep = n_distinct(sleep_new$new_id))

long <- participation %>% 
  pivot_longer(weight:sleep, names_to = "type", values_to = "count")

ggplot(long, aes(x= type, y = count, size = 4)) +
  geom_point() + 
  labs(title = "Entries by type", subtitle = "Number of entries for 30 day period by activity type")

ggsave("entries_by_type.png")

act_sleep_merge <- left_join(daily_new, sleep_new)

z_total_merge <- left_join(act_sleep_merge, weight_new)

head(z_total_merge)
colnames(z_total_merge)

summary(z_total_merge)

z_total_merge %>% 
  group_by(total_minutes_asleep >480) %>% 
  tally()

z_total_merge_2 <-  z_total_merge %>% 
  select(date, new_id, total_steps, total_distance, tracker_distance, logged_activities_distance, very_active_distance, moderately_active_distance, light_active_distance,
              sedentary_active_distance, very_active_minutes, fairly_active_minutes, lightly_active_minutes, sedentary_minutes, calories, total_sleep_records, wt_entry)

skim_without_charts(z_total_merge_2)

merge_2_long <- z_total_merge_2 %>% 
  pivot_longer(cols = total_steps:wt_entry, names_to = "Type", values_to = "Count", values_drop_na = TRUE)



Num_record_sorted <- merge_2_long %>% 
  group_by(date, Type) %>% 
  tally() %>% 
  rename(Date = date) %>% 
  rename(Counts = n)

tally <- z_total_merge_2 %>%
  pivot_longer(cols = total_steps:wt_entry, names_to = "Type", values_to = "Count", values_drop_na = TRUE) %>%
  tally() %>%
  rename(Date = date) %>%
  rename(Counts = n)


head(Num_record_sorted)
summary(Num_record_sorted)


ggplot(Num_record_sorted, aes(Date, fill = Type)) + geom_jitter(aes(x=Date, y= Counts, color = Type)) + 
  labs(title = "Number of Entries per day, by Type", subtitle = "Over 30 days", caption = "Data from FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius)") +
  guides(col = guide_legend(ncol = 2))


  
ggplot(Num_record_sorted) + geom_jitter(aes(Counts, y= Type, color = Type, )) + 
  labs(title = "Number of Entries per day, by Type", subtitle = "Over 30 days", 
       caption = "Data from FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius)") +
  scale_y_discrete(position = "right") + theme(legend.position = "none")

ggplot(Num_record_sorted, mapping = aes(x= Date, y= Counts)) + geom_col() +
  labs(title = "Number of Entries per day, by Type", subtitle = "Over 30 days", 
       caption = "Data from FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius)")
  

ggplot(Num_record_sorted, mapping = aes(x= Date, y= Counts), fun = distinct(Date)) + geom_col() 

ggplot(z_long, aes(date, fill = Type)) +
  geom_histogram()

ggplot(z_long, aes(date, colour = Type, position = "jitter")) +
  geom_freqpoly()

ggplot(Num_record_sorted, aes(Date, colour = Type, jitter(Counts))) +
  geom_freqpoly()


Num_record_sorted %>% 
  group_by(Date) %>% 
  tally() %>% 
  print(n=31)
  
sleep_by_date <- Num_record_sorted %>% 
  group_by(Date) %>% 
  filter(Type == "total_sleep_records")

sleep_user_date <- merge_2_long %>% 
  group_by(date, new_id) %>% 
  filter(Type == "total_sleep_records")

tally_sleep <- sleep_user_date %>% 
  group_by(new_id, date) %>% 
  tally()
  

ggplot(tally_sleep, aes(x=date, y= new_id, color = n)) + geom_col()

ggplot(sleep_by_date, aes(x=Date, y=Counts)) + geom_col() + facet_wrap()

ggplot(merge_2_long, aes(x=date, y=Count, fill = new_id)) + geom_col()

ggplot(merge_2_long, aes(x=date, y=Count, color = new_id)) + geom_point()

user_records <-merge_2_long %>% 
  group_by(new_id, date) %>% 
  tally() %>% 
  rename(Id = new_id) %>% 
  rename(Date = date) %>% 
  rename(Counts = n)

summary(daily_activity)
summary(daily_sleep)
summary(weight_log)

Num_record_sorted %>% 
  filter(type = total_sleep_records)

Num_record_sorted %>% 
  group_by(Date) %>% 
  tally() %>% 
  print(n=31)

sleep_by_date <- Num_record_sorted %>% 
  group_by(Date) %>% 
  filter(Type == "total_sleep_records")

sleep_user_date <- merge_2_long %>% 
  group_by(date, new_id) %>% 
  filter(Type == "total_sleep_records")

tally_sleep <- sleep_user_date %>% 
  group_by(new_id, date) %>% 
  tally()

sleep_new %>% 
  group_by(new_id) %>% 
  tally()

tally_sleep_2 <- sleep_new %>% 
  group_by(new_id, date) %>% 
  tally()

ggplot(tally_sleep, aes(x=new_id, y= n)) + geom_col()

ggplot(sleep_new, aes(x=date, y=total_sleep_records)) + geom_col() +
  labs(title = "Sleep Entries per Day", subtitle = "Over 30 days", 
       caption = "Data from FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius)")






