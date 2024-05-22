library(nycflights13)
install.packages("dplyr", repos = "https://cloud.r-project.org")
library(dplyr)
data("flights")

# View the first few rows of the dataset
head(flights)

# Summary of the dataset
summary(flights)

# Structure of the dataset
str(flights)

most_delayed_flights <- flights %>%
  group_by(dest) %>%                         # Group by destination
  filter(dep_delay == max(dep_delay, na.rm = TRUE)) %>% # Filter the max departure delay in each group
  ungroup()                                  # Ungroup the data

# View the result
print(most_delayed_flights)

# to make more readable subset for results ----->
most_delayed_flights %>%
  select(year, month, day, dep_time, sched_dep_time, dep_delay, arr_time, sched_arr_time, dest)


library(ggplot2)


# Extract the hour of departure
flights <- flights %>%
  mutate(dep_hour = floor(dep_time / 100)) %>%
  filter(!is.na(dep_delay))

# Calculate average departure delay for each hour
avg_delay_per_hour <- flights %>%
  group_by(dep_hour) %>%
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(dep_hour)

# Create the plot
ggplot(avg_delay_per_hour, aes(x = dep_hour, y = avg_dep_delay)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Average Departure Delay by Hour of the Day",
    x = "Hour of the Day",
    y = "Average Departure Delay (minutes)"
  ) +
  theme_minimal()


# Import necessary libraries
install.packages("styler")
library(styler)
library(tidyverse)


# Process the flights data for destination IAH
flights_IAH_summary <- flights |> 
  filter(dest == "IAH") |> 
  group_by(year, month, day) |> 
  summarize(
    n = n(),
    delay = mean(arr_delay, na.rm = TRUE)
  ) |> 
  filter(n > 10)

# View the result
print(flights_IAH_summary)


# Filter data --------------------------------------

# Process the flights data for UA carrier with specific conditions
flights_UA_filtered <- flights |> 
  filter(
    carrier == "UA",
    dest %in% c("IAH", "HOU"),
    sched_dep_time > 900,
    sched_arr_time < 2000
  )

# Summarize data --------------------------------------

# Group by flight and summarize the delays and cancellations
flights_UA_summary <- flights_UA_filtered |> 
  group_by(flight) |> 
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    cancelled = sum(is.na(arr_delay)),
    n = n()
  ) |> 
  filter(n > 10)

# View the result --------------------------------------

print(flights_UA_summary)


