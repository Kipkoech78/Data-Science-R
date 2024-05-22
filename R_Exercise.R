# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create the scatterplot
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(shape = 24, fill = "pink", size = 3)


# question 7 exercise9.4.1
# original code
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

# Recreating the plot using facet_wrap
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~ drv, ncol = 1)

library(ggplot2)
# Create a boxplot of highway fuel efficiency (hwy) grouped by drive train (drv)
ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_boxplot()


# ex 9.7.1
# Create a stacked bar chart
stacked_bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = clarity), 
    position = "fill",  # Stack the bars
    width = 1
  ) + 
  theme_minimal()

# Convert stacked bar chart into a pie chart using coord_polar()
pie_chart <- stacked_bar + coord_polar()

# Display the pie chart
print(pie_chart)


# ex 10.3.3
library(ggplot2)
library(dplyr)

# Plot histograms for x, y, and z variables
ggplot(diamonds, aes(x = x)) + 
  geom_histogram(binwidth = 0.1) + 
  ggtitle("Distribution of x (length)")

ggplot(diamonds, aes(x = y)) + 
  geom_histogram(binwidth = 0.1) + 
  ggtitle("Distribution of y (width)")


# Count diamonds at 0.99 carat and 1 carat
diamonds_0_99 <- diamonds %>% filter(carat == 0.99) %>% nrow()
diamonds_1_00 <- diamonds %>% filter(carat == 1.00) %>% nrow()

diamonds_0_99
diamonds_1_00

# ex 11.2.1

# Create the plot with customized labels
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +  # Scatter plot with points colored by car class
  geom_smooth(se = FALSE) +  # Add a smooth curve without confidence interval
  labs(
    x = "Engine Displacement (Liters)",
    y = "Highway Fuel Economy (MPG)",
    color = "Car Type",
    title = "Fuel Efficiency Trends by Engine Size",
    subtitle = "Larger engines generally have lower highway fuel efficiency, except for certain classes",
    caption = "Data Source: fueleconomy.gov"
  ) +
  theme_minimal()  # Apply a minimal theme for better aesthetics

# ex 11.4.6

# Initial plot
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1/20)

# Modified plot with override.aes
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1/20) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))

# exercise 11.5.1
install.packages("ggthemes")


library(ggthemes)

# Plot with ggthemes theme and customized axis labels
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  labs(
    title = "Larger engine sizes tend to have lower fuel economy",
    caption = "Source: https://fueleconomy.gov."
  ) +
  theme_bw() +  # Applying the ggthemes "few" theme
  theme(
    axis.text.x = element_text(color = "blue", face = "bold"),  # Customize x-axis label
    axis.text.y = element_text(color = "blue", face = "bold")   # Customize y-axis label
  )

mystring <-"He said \"That's amazing!\""
print(mystring)

# ex14.2.5
install.packages("babynames")
library(stringr)
library(babynames)

# Function to extract the middle character(s)
extract_middle <- function(name) {
  len <- str_length(name)
  if (len %% 2 == 1) {
    mid_pos <- (len %/% 2) + 1
    str_sub(name, mid_pos, mid_pos)
  } else {
    mid_pos1 <- len %/% 2
    mid_pos2 <- mid_pos1 + 1
    str_sub(name, mid_pos1, mid_pos2)
  }
}

# Apply the function to the babynames dataset
babynames_with_middle <- babynames %>%
  mutate(middle = sapply(name, extract_middle))

# Display the results
head(babynames_with_middle)

#exercise 15.3.5
library(stringr)

# Create a named vector mapping uppercase letters to lowercase letters
lowercase_map <- setNames(letters, LETTERS)

# Function to convert a string to lowercase
str_to_lower_simple <- function(x) {
  str_replace_all(x, lowercase_map)
}

# Test the function
test_strings <- c("Hello World!", "R for Data Science", "This is a TEST")
str_to_lower_simple(test_strings)

library(stringr)
data("words", package = "stringr")

# Words that start with "y"
words_start_y <- str_subset(words, "^y")
words_start_y
#____________________________________________________

# Words that don't start with "y"
words_not_start_y <- str_subset(words, "^[^y]")
words_not_start_y

#___________________________-

# Words that end with "x"
words_end_x <- str_subset(words, "x$")
words_end_x

#_____________________________________
# Words that are exactly three letters long
words_three_letters <- str_subset(words, "^[A-Za-z]{3}$")
words_three_letters

#__________________________________
# Words that have seven letters or more
words_seven_or_more <- str_subset(words, "^[A-Za-z]{7,}$")
words_seven_or_more
#___________________________________________________
# Words that contain a vowel-consonant pair
words_vowel_consonant <- str_subset(words, "[aeiou][bcdfghjklmnpqrstvwxyz]")
words_vowel_consonant
#______________________________________-
# Words that contain at least two vowel-consonant pairs in a row
words_two_vc_pairs <- str_subset(words, "([aeiou][bcdfghjklmnpqrstvwxyz]){2}")
words_two_vc_pairs
#___________________________________________
# Words that only consist of repeated vowel-consonant pairs
words_only_vc_pairs <- str_subset(words, "^([aeiou][bcdfghjklmnpqrstvwxyz])+$")
words_only_vc_pairs



