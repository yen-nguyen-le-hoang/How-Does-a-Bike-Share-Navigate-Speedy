# load packages
library(readr) #helps import data
library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle date attributes
library(janitor) #helps clean data
library(timechange) #helps manipulation of Date-Times
library(dplyr) #helps clean data
library(ggplot2) #helps visualize data
library(patchwork) #helps combine charts
library(gridExtra)
library(grid)
Sys.setlocale("LC_TIME", "C") #setting English  

# import data
Mar_24 <- read_csv("D:/11. DA/Foudation data data and data everwhere/202403-divvy-tripdata.csv")
Apr_24 <- read_csv("D:/11. DA/Foudation data data and data everwhere/202404-divvy-tripdata.csv")
May_24 <- read_csv("D:/11. DA/Foudation data data and data everwhere/202405-divvy-tripdata.csv")
Jun_24 <- read_csv("D:/11. DA/Foudation data data and data everwhere/202406-divvy-tripdata.csv")
Jul_24 <- read_csv("D:/11. DA/Foudation data data and data everwhere/202407-divvy-tripdata.csv")
Aug_24 <- read_csv("D:/11. DA/Foudation data data and data everwhere/202408-divvy-tripdata.csv")
Sep_24 <- read_csv("D:/11. DA/Foudation data data and data everwhere/202409-divvy-tripdata.csv")
Oct_24 <- read_csv("D:/11. DA/Foudation data data and data everwhere/202410-divvy-tripdata.csv")
Nov_24 <- read_csv("D:/11. DA/Foudation data data and data everwhere/202411-divvy-tripdata.csv")
Dec_24 <- read_csv("D:/11. DA/Foudation data data and data everwhere/202412-divvy-tripdata.csv")
Jan_25 <- read_csv("D:/11. DA/Foudation data data and data everwhere/202501-divvy-tripdata.csv")
Feb_25 <- read_csv("D:/11. DA/Foudation data data and data everwhere/202502-divvy-tripdata.csv")

# check cols
colnames(Mar_24)
colnames(Apr_24)
colnames(May_24)
colnames(Jun_24)
colnames(Jul_24)
colnames(Aug_24)
colnames(Sep_24)
colnames(Oct_24)
colnames(Nov_24)
colnames(Dec_24)
colnames(Jan_25)
colnames(Feb_25)

# combine 12 data files into 1 data frame
Total_trip <- rbind(Mar_24, Apr_24, May_24, Jun_24, Jul_24, Aug_24, Sep_24, Oct_24, Nov_24, Dec_24, Jan_25, Feb_25)
str(Total_trip)

# cleaning and manipulate data
Total_trip_1 <- na.omit(Total_trip) #drop na/null values
Total_trip_1$ride_length <- difftime(Total_trip_1$ended_at, Total_trip_1$started_at, units="mins") #create trip duration
str(Total_trip_1) #check type of vars
Total_trip_1$ride_length <- as.numeric(as.character(Total_trip_1$ride_length)) #covert to numeric 
is.numeric(Total_trip_1$ride_length)
Total_trip_2 <- Total_trip_1[!(Total_trip_1$ride_length <1|Total_trip_1$ride_length > 1440),] #remove ‘unwanted bad’ data

Total_trip_2 <- mutate(Total_trip_2, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at)) #convert "started_at" and "ended_at" to POSIXct (date-time) to extract hour, date, day, month, year
Total_trip_2$date <- as.Date(Total_trip_2$started_at) #extract date
Total_trip_2$days_of_week <- format(as.Date(Total_trip_2$date), "%A") #extract days_of_week
unique(Total_trip_2$days_of_week)
Total_trip_2$days_of_week <- factor(Total_trip_2$days_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
is.factor(Total_trip_2$days_of_week)
Total_trip_2$month_year <- format(Total_trip_2$started_at, "%b-%Y") #extract "month_year"
ordered_levels <- c("Mar-2024", "Apr-2024", "May-2024", "Jun-2024", "Jul-2024",
                    "Aug-2024", "Sep-2024", "Oct-2024", "Nov-2024", "Dec-2024",
                    "Jan-2025", "Feb-2025")
Total_trip_2$month_year <- factor(Total_trip_2$month_year, levels = ordered_levels, ordered = TRUE)
Total_trip_2$start_hour <- format(as.POSIXct(Total_trip_2$started_at), format = "%H") #extract start_hour
View(Total_trip_2)

Total_trip_3 <- Total_trip_2[!duplicated(Total_trip_2$ride_id),] #remove duplicate  
dim(Total_trip_3)
glimpse(Total_trip_3)

# One final check before analyzing
colSums(is.na(Total_trip_3))
sum(duplicated(Total_trip_3$ride_id))
View(filter(Total_trip_3, Total_trip_3$started_at > Total_trip_3$ended_at))
View(filter(Total_trip_3, Total_trip_3$ride_length <1|Total_trip_3$ride_length > 1440))

## final data frame to analyze named: Total_trip_3, include 18 vars and 1.084.919 obs

# statistic summary ride_length
summary(Total_trip_3$ride_length)
Total_trip_3 %>%
  group_by(member_casual) %>%
  summarise(
    mean = mean(ride_length),
    median = median(ride_length),
    max = max(ride_length),
    min = min(ride_length)
  )


# The number of rides per month
bar_chart_mon <- Total_trip_3 %>%
  group_by(member_casual, month_year) %>%
  summarise(
    number_of_rides = n(),
    average_duration = mean(ride_length),
    .groups = "drop"
  ) %>%
  arrange(member_casual, month_year) %>%
  ggplot(aes(x = month_year, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Pic1: Number of Rides by Month and Member Type",
       x = "Month-Year", y = "Number of Rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
table_data_mon <- Total_trip_3 %>%
  group_by(member_casual, month_year) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  pivot_wider(
    names_from = month_year,
    values_from = number_of_rides,
    values_fill = 0
  )
names(table_data_mon) <- make.names(names(table_data_mon), unique = TRUE)
small_font_theme <- ttheme_minimal(
  core = list(fg_params = list(cex = 0.4)),
  colhead = list(fg_params = list(cex = 0.5))
)
table_plot_mon <- tableGrob(table_data_mon, theme = small_font_theme)
combined_plot_mon <- bar_chart_mon / table_plot_mon
print(combined_plot_mon)

## top 10 days
daily_rides <- Total_trip_3 %>%
  group_by(date, member_casual) %>%
  summarise(number_of_rides = n(), .groups = 'drop')
top_10_days <- daily_rides %>%
  group_by(member_casual) %>%
  slice_max(order_by = number_of_rides, n = 10)
print(top_10_days)


# The number of rides per day
bar_chart_day <- Total_trip_3 %>% 
  mutate(days_of_week = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, days_of_week) %>% 
  summarise(
    number_of_rides = n(),
    average_duration = mean(ride_length),
    .groups = "drop"
  ) %>% 
  arrange(member_casual, days_of_week) %>% 
  ggplot(aes(x = days_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Pic2: Number of Rides by Day and Member Type",
       x = "Day of Week", y = "Number of Rides") +
  theme_minimal()
table_data_day <- Total_trip_3 %>%
  mutate(days_of_week = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, days_of_week) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  pivot_wider(
    names_from = days_of_week,
    values_from = number_of_rides,
    values_fill = 0
  )
table_plot_day <- tableGrob(table_data_day)
names(table_data_day) <- make.names(names(table_data_day), unique = TRUE)
combined_plot_day <- bar_chart_day / table_plot_day
print(combined_plot_day)


# The number of rides per hour
bar_chart_hour <- Total_trip_3 %>%
  group_by(member_casual, start_hour) %>%
  summarise(
    number_of_rides = n(),
    average_duration = mean(ride_length),
    .groups = "drop"
  ) %>%
  arrange(member_casual, start_hour) %>%
  ggplot(aes(x = start_hour, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Pic3: Number of Rides by Start Hour and Member Type",
       x = "Start Hour", y = "Number of Rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
heatmap_data <- Total_trip_3 %>%
  group_by(member_casual, start_hour) %>%
  summarise(number_of_rides = n(), .groups = "drop")
heatmap_hour <- ggplot(heatmap_data, aes(x = factor(start_hour), y = member_casual, fill = number_of_rides)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Heatmap",
       x = "Start Hour", y = "Member Type", fill = "Number of Rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
library(ggplotify)
heatmap_pp_hour <- as.ggplot(heatmap_hour)
combined_plot_hour <- bar_chart_hour/heatmap_pp_hour
print(combined_plot_hour)

## compare Rides by Start Hour on Weekends and on Weekdays
plot_weekday <- Total_trip_3 %>%
  filter(days_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) %>%
  ggplot(aes(x = start_hour, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Pic5: Rides by Start Hour on Weekdays",
    x = "Start Hour",
    y = "Number of Rides",
    fill = "Member Type"
  ) +
  scale_fill_manual(values = c("casual" = "salmon", "member" = "turquoise")) +
  theme_minimal()
plot_weekend <- Total_trip_3 %>%
  filter(days_of_week %in% c("Saturday", "Sunday")) %>%
  ggplot(aes(x = start_hour, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Pic6: Rides by Start Hour on Weekends",
    x = "Start Hour",
    y = "Number of Rides",
    fill = "Member Type"
  ) +
  scale_fill_manual(values = c("casual" = "salmon", "member" = "turquoise")) +
  theme_minimal()
plot_weekday + plot_weekend + plot_layout(ncol = 1)

## pie chart compare Rides by Start Hour on Weekends and on Weekdays
pie1_data <- Total_trip_3 %>%
  filter(member_casual == "member") %>%
  mutate(day_type = ifelse(wday(started_at, label = TRUE) %in% c("Sat", "Sun"), "Weekend", "Weekday")) %>%
  count(day_type)
pie1 <- ggplot(pie1_data, aes(x = "", y = n, fill = day_type)) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(title = "Member") +
  theme_void() +
  scale_fill_brewer(palette = "Set1")
pie2_data <- Total_trip_3 %>%
  filter(member_casual == "casual") %>%
  mutate(day_type = ifelse(wday(started_at, label = TRUE) %in% c("Sat", "Sun"), "Weekend", "Weekday")) %>%
  count(day_type)
pie2 <- ggplot(pie2_data, aes(x = "", y = n, fill = day_type)) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(title = "Casual") +
  theme_void() +
  scale_fill_brewer(palette = "Set1")
pie3_data <- Total_trip_3 %>%
  filter(!(wday(started_at, label = TRUE) %in% c("Sat", "Sun"))) %>%
  count(member_casual)
pie3 <- ggplot(pie3_data, aes(x = "", y = n, fill = member_casual)) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(title = "Weekday") +
  theme_void() +
  scale_fill_brewer(palette = "Dark2")
pie4_data <- Total_trip_3 %>%
  filter(wday(started_at, label = TRUE) %in% c("Sat", "Sun")) %>%
  count(member_casual)
pie4 <- ggplot(pie4_data, aes(x = "", y = n, fill = member_casual)) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(title = "Weekend") +
  theme_void() +
  scale_fill_brewer(palette = "Dark2")
(pie1 | pie2) / (pie3 | pie4)

# Compare trip_count and trip_duration
trip_count <- Total_trip_3 %>%
  count(member_casual) %>%
  mutate(percentage = round(100 * n / sum(n), 1),
         label = paste0(member_casual, ": ", percentage, "%"))
plot1 <- ggplot(trip_count, aes(x = "", y = n, fill = member_casual)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Total Trips by Rider Type") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme_void() +
  scale_fill_brewer(palette = "Set2")
trip_duration <- Total_trip_3 %>%
  group_by(member_casual) %>%
  summarise(total_duration = sum(ride_length, na.rm = TRUE)) %>%
  mutate(percentage = round(100 * total_duration / sum(total_duration), 1),
         label = paste0(member_casual, ": ", percentage, "%"))
plot2 <- ggplot(trip_duration, aes(x = "", y = total_duration, fill = member_casual)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Total Trip Duration by Rider Type") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme_void() +
  scale_fill_brewer(palette = "Set2")
plot1 + plot2

# Analyze Ride Duration by Day and Member Type:
bar_chart_duration <- Total_trip_3 %>% 
  mutate(days_of_week = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, days_of_week) %>% 
  summarise(
    number_of_rides = n(),
    average_duration = mean(ride_length),
    .groups = "drop"
  ) %>% 
  arrange(member_casual, days_of_week) %>% 
  ggplot(aes(x = days_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Pic7: Average Ride Duration by Day and Member Type",
       x = "Day of Week", y = "Average Duration (minutes)") +
  theme_minimal()
table_data_duration <- Total_trip_3 %>%
  mutate(days_of_week = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, days_of_week) %>%
  summarise(average_duration = round(mean(ride_length), 2), .groups = 'drop') %>%
  pivot_wider(
    names_from = days_of_week,
    values_from = average_duration,
    values_fill = 0
  )
names(table_data_duration) <- make.names(names(table_data_duration), unique = TRUE)
table_plot_duration <- tableGrob(table_data_duration)
combined_duration <- bar_chart_duration / table_plot_duration
print(combined_duration)

# What type of bike casual riders and members prefer
Total_trip_3 %>% 
  group_by(member_casual, rideable_type) %>% 
  arrange(member_casual, rideable_type)  %>% 
  ggplot(aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge")

# code to save data.frame if you want: .csv or .rds or .feather 
# getwd()

# write.csv(Total_trip_3, "Total_trip_3.csv", row.names = FALSE)

# saveRDS(Total_trip_3, "Total_trip_3.rds")

# library(arrow)
# write_feather(Total_trip_3, "Total_trip_3.feather")
