library(dplyr)
library(tidyverse)

# Step 1: Extract TB cases
cases <- table2 %>%
  filter(type == "cases") %>%
  select(country, year, cases = count)

# Step 2: Extract population
population <- table2 %>%
  filter(type == "population") %>%
  select(country, year, population = count)

# Step 3: Join cases and population, then calculate the rate
rate_table2 <- cases %>%
  inner_join(population, by = c("country", "year")) %>%
  mutate(rate = cases / population * 10000)

rate_table2

# Load necessary libraries
library(dplyr)
library(tidyr)

# Create table2
table2 <- tibble(
  country = c("Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", "Brazil", "Brazil", "Brazil", "Brazil", "China", "China", "China", "China"),
  year = c(1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000),
  type = c("cases", "population", "cases", "population", "cases", "population", "cases", "population", "cases", "population", "cases", "population"),
  count = c(745, 19987071, 2666, 20595360, 37737, 172006362, 80488, 174504898, 212258, 1272915272, 213766, 1280428583)
)

# Create table3
table3 <- tibble(
  country = c("Afghanistan", "Afghanistan", "Brazil", "Brazil", "China", "China"),
  year = c(1999, 2000, 1999, 2000, 1999, 2000),
  rate = c("745/19987071", "2666/20595360", "37737/172006362", "80488/174504898", "212258/1272915272", "213766/1280428583")
)
#________________Calculate the rate for table2
# Step 1: Extract TB cases
cases <- table2 %>%
  filter(type == "cases") %>%
  select(country, year, cases = count)

# Step 2: Extract population
population <- table2 %>%
  filter(type == "population") %>%
  select(country, year, population = count)

# Step 3: Join cases and population, then calculate the rate
rate_table2 <- cases %>%
  inner_join(population, by = c("country", "year")) %>%
  mutate(rate = cases / population * 10000)

# View the result
rate_table2


# Step 5: Store the results in the original format of table2
table2_with_rate <- table2 %>%
  filter(type == "cases") %>%
  left_join(cases_population %>% select(country, year, rate), by = c("country", "year")) %>%
  select(country, year, type, count, rate)

# Print the final table with rates
print(table2_with_rate)




# Join cases and population data
cases_population <- left_join(cases, population, by = c("country", "year"))

# Calculate the rate
cases_population <- cases_population %>%
  mutate(rate = cases / population * 10000)

# Display the results
cases_population



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




