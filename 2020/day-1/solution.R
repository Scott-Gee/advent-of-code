library(dplyr)
library(tidyr)
library(here)

source(here('2020', 'day-1', 'data.R'))

tibble(value = list) %>%  # Create tibble from list of numbers
  expand(value, 
         value1 = value) %>% # Get every combination of numbers in two columns
  filter(value < value1) %>% # Remove duplicates
  mutate(sum = value + value1) %>%  # Add together
  filter(sum == 2020) %>% # Keep rows that sum to 2020
  mutate(product = value * value1) %>% # Multiply columns
  pull(product) # Get answer

tibble(value = list) %>% # Create tibble from list of numbers
  expand(value,
         value1 = value,
         value2 = value) %>% # Get every combination of numbers in three columns
  filter(value < value1,
         value < value2,
         value1 < value2) %>% # Remove duplicates
  mutate(sum = value + value1 + value2) %>% # Add together
  filter(sum == 2020) %>% # Keep rows that sum to 2020
  mutate(product = value * value1 * value2) %>% 
  pull(product) # Get answer

