install.packages(c("readxl","dplyr","ggplot2"))
library(readxl)
library(dplyr)
library(ggplot2)

p19 <- "C:/Users/frajz/OneDrive/Desktop/Divvy_Trips_2020 +2019/Divvy_Trips_2019_Q1.xlsx"
p20 <- "C:/Users/frajz/OneDrive/Desktop/Divvy_Trips_2020 +2019/Divvy_Trips_2020_Q1.xlsx"

df2019 <- read_excel(p19)
df2020 <- read_excel(p20)

# RENAMING AND CHOOSING THE RIGHT COLUMNS FOR ANALYSIS
df2019 <- df2019 %>%
  rename(
    started_at         = start_time,
    ended_at           = end_time,
    start_station_name = from_station_name,
    end_station_name   = to_station_name
  ) %>%
  mutate(member_casual = recode(usertype,
                                "Subscriber" = "member",
                                "Customer"   = "casual")) %>%
  select(started_at, ended_at, start_station_name, end_station_name, member_casual)

df2020 <- df2020 %>%
  select(started_at, ended_at, start_station_name, end_station_name, member_casual)

# MERGING BOTH DATASETS INTO 1
all_trips <- bind_rows(df2019, df2020)

all_trips <- all_trips %>%
  mutate(
    ride_length_min = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week     = weekdays(started_at),
    day_of_week     = factor(day_of_week,
                             levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  ) %>%
  filter(ride_length_min > 0)

#SUMMARIES
avg_len <- all_trips %>%
  group_by(member_casual) %>%
  summarise(avg_min = mean(ride_length_min), .groups = "drop")

rides_by_dow <- all_trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(rides = n(), .groups = "drop")

# === PLOTS ===
ggplot(avg_len, aes(member_casual, avg_min, fill = member_casual)) +
  geom_col() +
  labs(title = "Average ride length (minutes)", x = "Rider type", y = "Avg minutes") +
  guides(fill = "none")

ggplot(rides_by_dow, aes(day_of_week, rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Rides by weekday", x = "Weekday", y = "Rides")

