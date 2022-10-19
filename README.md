# Bellabeat Case Study - Google Data Analytics Capstone

1. [Phase 1: ASK](#1)
2. [Phase 2: Prepare](#2)
3. [Phase 3: Process](#3)<br>
    3.1 [Clean](#3.1)<br>
    3.2 [Merge](#3.2)
4. [Phase 4: Analyze](#4)<br>
    4.1 [Summary](#4.1)
5. [Phase 5: Share](#5)<br>
    5.1 [Trends](#5.1)<br>
      5.1.1 [Participation](#5.1.1)<br>
      5.1.2 [Tracking](#5.1.2)<br>
      5.1.3 [Sleep](#5.1.3)
    
## About the Company

Bellabeat, a high-tech company that manufactures health-focused smart products. By collecting data on activity, sleep, stress, and reproductive health, Bellabeat seeks to empower women with knowledge about their own health and habits. Using this consumer data to reveal more opportunities for growth and high-level recommendations for how these trends can inform Bellabeat marketing strategy.

<a id="1"></a> <br>
### **PHASE ONE: ASK**

**Questions for the analysis**

-   What are some trends in smart device usage?
-   How could these trends apply to Bellabeat customers?

**Business task**

Identify trends in how customers are using the smart devices to determine potential opportunities for growth and recommendations for the Bellabeat marketing strategy.

<a id="2"></a> <br>
### **PHASE TWO: PREPARE**

**About the data**

The data was downloaded to a personal computer and saved in a dedicated folder for this project. The data set used is public domain and has been uploaded to [Kaggle](https://www.kaggle.com/datasets/arashnic/fitbit) by user Mobius. 

This data set consists of 30 eligible Fitbit users who consented to the submission of their personal device data, including minute-level output for physical activity, heart rate, and sleep monitoring. Some files included a smaller subset(weight) that was included as it speaks to the trends in device usage.

For the most part, the data is acquired automatically from the devices and should be fairly reliable. The weight data also includes some manual entries, which should not be problematic at the specific figures are not being analyzed in this particular study and the weight entries are being considered only insofar as they speak to how users are using their devices in broad strokes.

The specific files from this source being used, daily activity, daily sleep, and weight, will focus primarily on daily tracking habits of users to provide trends for potential future growth and opportunities in marketing.

Limitations include: small sample size, inconsistent recording of activities, a lack of demographic information and inconsistency between data frames. 


#### Install Packages

```{r, install packages}

install.packages("tidyverse")
install.packages("janitor")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("lubridate")
install.packages("convertr")
```

#### Load Libraries

```{r load libraries}
library(tidyverse)
library(janitor)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(skimr)
library(lubridate)
library(convertr)
```

#### Import data sets

```{r}
daily_activity <- read.csv("~/Fitbit_dataset/dailyActivity_merged.csv")
daily_sleep <- read.csv("~/Fitbit_dataset/sleepDay_merged.csv")
weight_log <- read.csv("~/Fitbit_dataset/weightLogInfo_merged.csv")
```

**Preview data frames**

```{r View data frames}
head(daily_activity)
head(daily_sleep)
head(weight_log)

colnames(daily_activity)
colnames(daily_sleep)
colnames(weight_log)
```
![image](https://user-images.githubusercontent.com/113202250/196532546-e247b147-1a90-4613-ae9a-eab3309cdcda.png)
![image](https://user-images.githubusercontent.com/113202250/196532624-103c28da-f916-408f-bb8e-e0e338e5a463.png)
![image](https://user-images.githubusercontent.com/113202250/196532699-d1cb11cc-617c-4afb-87f6-92df15756768.png)

**Checking for errors and anomalies**

```{r check for errors or anomalies}
summary(daily_activity)
summary(daily_sleep)
summary(weight_log)
```
![image](https://user-images.githubusercontent.com/113202250/196532767-4bc3028a-7cce-4266-9bbc-31e515970acc.png)
![image](https://user-images.githubusercontent.com/113202250/196532828-cb0beabf-8079-45f7-b1f0-d35c44cb4e42.png)

<a id="3"></a> <br>
### **PHASE THREE: PROCESS**

We will focus on daily records for activity, sleep and weight tracking information to inform the stakeholders of the trends their users are exhibiting. Unnecessary columns from the sleep and weight data sets have been omitted as they do not add to the analysis and were able to be condensed into a smaller data set that included user id, date and one variable to allow for tallying of records.

We chose to complete this analysis using RStudio primarily, we utilized Googlesheets and BiqQuery to verify some of our functions throughout but ultimately RStudio accomplished everything we set out to do.

**Identifying Outliers and Errors**

```{r identifying outliers and errors}
daily_sleep %>% 
  group_by(TotalMinutesAsleep >480) %>% 
  tally()

daily_activity %>% 
  group_by(SedentaryMinutes > 960, TotalSteps <100) %>% 
  tally()
```
![image](https://user-images.githubusercontent.com/113202250/196532946-24b1b5e1-fec3-4ac5-b6a6-3acd401af6df.png)

Of note: There were several entries with sleep well above 480 minutes which would be 8 hours per day, while this does seem unusual the entries were not contained to a single user. There were also many entries with 16 hours or more of sedentary minutes, 82 of these also had fewer than 100 steps per day. There is not an adequate metric for disqualifying, nullifying these entries did not seem appropriate.

#### Clean up the data

```{r clean column names}
daily_activities <- clean_names(daily_activity)
daily_sleep_log <- clean_names(daily_sleep)
weights_log <- clean_names(weight_log)
```

**Preview the data**

```{r view cleaned column names}
colnames(daily_activities)
head(daily_activities)
glimpse(daily_activities)
```
![image](https://user-images.githubusercontent.com/113202250/196533089-a20f2d84-6257-4d70-897b-483c175487d2.png)


```{r}
colnames(daily_sleep_log)
head(daily_sleep_log)
glimpse(daily_sleep_log)
```
![image](https://user-images.githubusercontent.com/113202250/196533177-547856f2-15b0-4626-a9e7-0921470b4441.png)


```{r}
colnames(weights_log)
head(weights_log)
glimpse(weights_log)
```
![image](https://user-images.githubusercontent.com/113202250/196533232-7e074b17-8f62-4757-b00b-c4079fcd3f62.png)

<a id="3.1"></a> <br>
##### Clean up dates and data types, create new data frames

Cleaning up the data types and dates for simpler merging and easier reading. We changed ids to characters so they weren't being converted to exponents during analysis, we standardized the date formats and we selected the relevant columns to keep in the new data frame.

Most of the weight log columns were removed as they didn't provide meaningful context as weight fluctuations are normal from day to day and any meaningful trends would require a longer survey of data, since most users did not track weight and were sporadic, we kept one column to support a comparison for how the devices were used.

```{r clean up dates and data types, create updated data files}
sleep_new <- daily_sleep_log %>% 
  mutate(new_id = as.character(id)) %>% 
  mutate(date = as.Date(sleep_day, "%m/%d/%Y %H:%M:%S")) %>% 
  select(new_id, date, total_sleep_records)
  
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
```

<a id="3.2"></a> <br>
#### Merge the data frames

We used a left join so that all of the data merged together, so that we could accurately analyze participation in tracking various activities.

**Merging**

```{r merge daily activity and sleep and join with weight}
act_sleep_merge <- left_join(daily_new, sleep_new)

z_total_merge <- left_join(act_sleep_merge, weight_new) %>% 
  na_if(0)
```
![image](https://user-images.githubusercontent.com/113202250/196533383-c7e0e67c-0875-44ce-96db-e218fd01d86d.png)


**Preview the new data frame**

```{r preview the new data frame}
head(z_total_merge)
colnames(z_total_merge)
```
![image](https://user-images.githubusercontent.com/113202250/196533439-a87cc55c-7b63-4ceb-b122-6701f1ff133c.png)

<a id="4"></a> <br>
### **PHASE FOUR: ANALYZE**

**Clean up merged data frame**

**Rearrange data frame**

The original merged file was difficult to read with the dates and ids in the middle of the columns.

```{r Rearrange columns simpler viewing}
z_total_merge_2 <-  z_total_merge %>% 
  select(date, new_id, total_steps, total_distance, tracker_distance, logged_activities_distance, very_active_distance, 
  moderately_active_distance, light_active_distance, sedentary_active_distance, very_active_minutes, fairly_active_minutes, 
  lightly_active_minutes, sedentary_minutes, calories, total_sleep_records, wt_entry)
```

**Preview cleaned up merged file**

```{r skim}
skim_without_charts(z_total_merge_2)
```
![image](https://user-images.githubusercontent.com/113202250/196533686-11216b52-5630-4531-aefb-609ae11795eb.png)


#### Pivot longer for visualization

**Pivot the merged file for visualization**

Pivoting the data frame for visualization, we dropped the NA values so that they did not skew actively recorded entry tallies.

```{r pivot longer}
merge_2_long <- z_total_merge_2 %>% 
  pivot_longer(cols = total_steps:wt_entry, names_to = "Type", values_to = "Count", values_drop_na = TRUE)
```

**Tally for number of entries per day, per type**

```{r tally how many entries per day, per type}
tally_date <- z_total_merge_2 %>%
  pivot_longer(cols = total_steps:wt_entry, names_to = "Type", values_to = "Count", values_drop_na = TRUE) %>%
  group_by(date, Type) %>% 
  tally() %>%
  rename(Date = date) %>% 
  rename(Counts = n)
```

```{r preview of tally}
head(tally_date)
summary(tally_date)
```
![image](https://user-images.githubusercontent.com/113202250/196533833-6d4baf23-d59f-4fbb-a504-ab5e90859911.png)


#### Most commonly tracked activity types

```{r tally how many entries per user, per type}
tally_user <- merge_2_long %>% 
  group_by(new_id, Type) %>% 
  tally() %>% 
  rename(ID = new_id) %>% 
  rename(Counts = n)
```

```{r preview}
head(tally_user)
summary(tally_user)
```
![image](https://user-images.githubusercontent.com/113202250/196533982-438d8d88-5301-4ef3-9b65-b4cde807516e.png)

<a id="4.1"></a> <br>
**Summary:** Our analysis indicates, that of the 30 users in the data set group, the majority were using their device to track physical activity. Many also tracked sleep but did not provide daily sleep data over the 30 day period. A small sample of the group tracked their weight and not daily.

<a id="5"></a> <br>
### PHASE FIVE: SHARE
<a id="5.1"></a> <br>
### **Trends**
<a id="5.1.1"></a> <br>
#### Participation

```{r Participation, echo=FALSE}
participation <- tibble(weight = n_distinct(weight_new$new_id), activity = n_distinct(daily_new$new_id), 
sleep = n_distinct(sleep_new$new_id))

long <- participation %>% 
  pivot_longer(weight:sleep, names_to = "type", values_to = "count")

ggplot(long, aes(x= type, y = count, size = 4)) +
  geom_point() + 
  labs(title = "Entries by type", subtitle = "Number of entries for 30 day period by activity type, per user")
```
![image](https://user-images.githubusercontent.com/113202250/196535824-368e9f5f-fa84-4edf-a2ec-29eb5127d130.png)

These plots show that users appear to primarily use their devices to track activity. Similar to the participation plot above this shows that the fewest number of entries were for weights, followed by sleep.

```{r, fig.width=15,fig.height=7, echo=FALSE}
tally_date %>% 
group_by(Type) %>%
  summarize(Counts = sum(Counts)) %>%
  ggplot(aes(Type, Counts)) +
  geom_col(aes(fill = Type)) +
  labs(title = "Number of Entries by Type for the Month", 
       caption = "Data from FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius)")+
  theme(legend.position = "none", axis.text.x = element_blank())+
  geom_text(aes(x = Type, y = Counts/2, label = Type), nudge_y = 75, colour = "black", angle = 320)
```
In the graphic below, logged_activities_distance is lower than weight entries. This appears to be due to logged activities being manual entries, which do not appear to be a priority for this particular group who prefers to track their movement automatically.
![image](https://user-images.githubusercontent.com/113202250/196541628-94affdb7-baa1-4586-bdb5-db4ddb25adf0.png)
```{r participation, fig.width=15,fig.height=7, echo=FALSE}
ggplot(tally_date) + geom_jitter(aes(Counts, y= Type, color = Type)) + 
  labs(title = "Number of Entries per day, by Type", subtitle = "Over 30 days", 
       caption = "Data from FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius)") +
  scale_y_discrete(position = "right") + theme(legend.position = "none")
```
![image](https://user-images.githubusercontent.com/113202250/196534207-f3de7126-077c-4382-aa9d-b7407de46b0e.png)

```{r  participation, fig.width=15,fig.height=7, echo=FALSE}
tally_user %>% 
  group_by(ID) %>%
  summarize(Counts = sum(Counts)) %>%
  ggplot(aes(ID, Counts)) +
  geom_col() + geom_label(aes(label = ID))+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(title = "Number of Entries by User for the Month", 
       caption = "Data from FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius)")
```
![image](https://user-images.githubusercontent.com/113202250/196534255-88b1e399-2be9-4a8b-90d8-609c75878498.png)


Upon further inspection of User specific data, it appears that user 4057192912 only tracked 27 data points. Further investigation shows this user only tracked data for April 12-15th. This user's data could probably be removed in a further analysis.

```{r}
user_4057192912 <- merge_2_long %>% 
  filter(new_id == 4057192912)

summary(user_4057192912)
```
![image](https://user-images.githubusercontent.com/113202250/196534311-4a837c5f-8261-4e93-9ac0-5c181057d47e.png)
<a id="5.1.2"></a> <br>
#### Diligence in tracking
Another potential trend worth noting is the decrease in entries overall towards the end of the data collection period, as seen below. This could indicate that while users were willing to give access to their data to the provider, for analysis, that they may have started the period off more engaged while losing some interest in tracking data towards the end of the 30 day period. This could indicate an external motivation for activity by many users, capabilities for competing with friends and strangers on challenges may motive users to engage and track more.

```{r Entries per day, fig.height=8, fig.width=14, echo=FALSE}

ggplot(tally_date, mapping = aes(x= Date, y= Counts)) + geom_col() +
  labs(title = "Number of Entries per day", subtitle = "Over 30 days", 
       caption = "Data from FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius)")
```
![image](https://user-images.githubusercontent.com/113202250/196534365-bcc18a1a-af5a-4b39-8b32-8d1e8ea6d6b7.png)
<a id="5.1.3"></a> <br>
#### Sleep tracking

One more trend noticed... sleep tracking appeared to fluctuate the most in cycles of approximately 3 days, one hypothesis is that users need to charge their devices and may be doing that while they are sleep so as to not lose out on tracking the information they are most interested in tracking. This hypothesis would need further exploration before any recommendations could be made specifically, however, a quick charge ability may help if this is in fact the case.

```{r sleep tracking trend, echo=FALSE}
ggplot(sleep_new, aes(x=date, y=total_sleep_records)) + geom_col() +
  labs(x= "Date", y= "Sleep Records", title = "Sleep Entries per Day", subtitle = "Over 30 days", 
  caption = "Data from FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius)")
```
![image](https://user-images.githubusercontent.com/113202250/196534423-5ad92d46-36cc-40b2-8589-ae07a915b132.png)


The questions we started with:

**-   What are some trends in smart device usage?**

  - Trends have been identified above in the "Trends" section.
  
**-   How could these trends apply to Bellabeat customers?**

  - These trends provide an opportunity for Bellabeat to create an interactive environment for their users where they are encouraged to move more and rest well without an emphasis on weight loss or gain.
