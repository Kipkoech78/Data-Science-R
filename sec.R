# Load the necessary library
library(tidyverse)

# Given table2 as described
table2 <- tibble(
  country = c("Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", "Brazil", "Brazil", "Brazil", "Brazil", "China", "China", "China", "China"),
  year = c(1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000),
  type = c("cases", "population", "cases", "population", "cases", "population", "cases", "population", "cases", "population", "cases", "population"),
  count = c(745, 19987071, 2666, 20595360, 37737, 172006362, 80488, 174504898, 212258, 1272915272, 213766, 1280428583)
)

# Step 1: Extract TB cases
cases <- table2 %>%
  filter(type == "cases") %>%
  select(country, year, cases = count)

# Step 2: Extract population
population <- table2 %>%
  filter(type == "population") %>%
  select(country, year, population = count)

# Step 3: Join cases and population tables
cases_population <- cases %>%
  left_join(population, by = c("country", "year"))

# Step 4: Calculate the rate
cases_population <- cases_population %>%
  mutate(rate = cases / population * 10000)

# Step 5: Store the results in the original format of table2
table2_with_rate <- table2 %>%
  filter(type == "cases") %>%
  left_join(cases_population %>% select(country, year, rate), by = c("country", "year")) %>%
  select(country, year, type, count, rate)

# Print the final table with rates
print(table2_with_rate)

# Given vector of days
x <- c("Monday", "Saturday", "Wednesday")

# Label days as weekends or weekdays
label <- if_else(x %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# Display the result
label

library(dplyr)

# Assuming 'flights' is the dataset containing flight information

# Calculate the proportion of cancelled flights for each plane
worst_on_time_record <- flights %>%
  group_by(tailnum) %>%
  summarize(prop_cancelled = mean(is.na(dep_time), na.rm = TRUE),
            total_flights = n()) %>%
  filter(!is.na(tailnum)) %>%  # Exclude flights with missing tailnum
  arrange(desc(prop_cancelled))  # Arrange by descending proportion of cancelled flights

# Display the plane with the worst on-time record
head(worst_on_time_record, n = 1)

library(ggplot2)

# Create a sample plot
p <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() 

# Add text at the four corners
p +
  geom_text(
    aes(x = Inf, y = -Inf, label = "Bottom Right"),
    hjust = 1, vjust = 0, size = 4, color = "blue"
  ) +
  geom_text(
    aes(x = Inf, y = Inf, label = "Top Right"),
    hjust = 1, vjust = 1, size = 4, color = "red"
  ) +
  geom_text(
    aes(x = -Inf, y = -Inf, label = "Bottom Left"),
    hjust = 0, vjust = 0, size = 4, color = "green"
  ) +
  geom_text(
    aes(x = -Inf, y = Inf, label = "Top Left"),
    hjust = 0, vjust = 1, size = 4, color = "orange"
  )
