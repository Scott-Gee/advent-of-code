library(dplyr)
library(readr)
library(here)

data <- read_table(here('2021', 'day-1', 'data.txt'), col_names = FALSE)

# Part 1
data %>% 
  mutate(larger_than_before = if_else(X1 > lag(X1), 1, 0)) %>%  # If the current value is larger than the previous value then 1 else 0
  summarise(total_larger = sum(larger_than_before, na.rm = TRUE)) # Sum the rows that are larger

# Part 2 
data %>% 
  mutate(X2 = lag(X1),
         X3 = lag(X2)) %>%  # Create extra columns for sliding sum
  filter(!is.na(X2),
         !is.na(X3)) %>% 
  mutate(sum = X1 + X2 + X3,
         larger_than_before = if_else(lag(sum) < sum, 1, 0)) %>% 
  summarise(total_larger = sum(larger_than_before, na.rm = TRUE))

         