library(dplyr)
library(readr)
library(here)
library(purrr)
library(tidyr)

data <- read_table(here('2021', 'day-5', 'data.txt'), col_names = FALSE) %>% 
  separate(X1, into = c('X1', 'X2'), sep = ' -> ') # Convert in to two columns separating on ->

## Part 1 
data %>% 
  separate(X1, into = c('x1', 'y1')) %>% # Separate coordinates for x1 and y1  
  separate(X2, into = c('x2', 'y2')) %>%# Separate coordinates for x2 and y2  
  filter(x1 == x2 | y1 == y2) %>% # Keep only horizontal or vertical lines
  mutate(x = map2(x1, x2, seq), # Get all numbers between x1 and x2
         y = map2(y1, y2, seq)) %>%  # Get all numbers between y1 and y2
  unnest(c(x, y)) %>%  # Unnest in to a dataframe
  count(x, y) %>% # Count the coordinates, if count is more than 1 then they overlap
  filter(n > 1) %>% # Return the coordinates that overlap
  count() # Count unique overlapping coordinates

## Part 2 - Same as Part 1 just no need to filter for just horizontal lines
data %>% 
  separate(X1, into = c('x1', 'y1')) %>% 
  separate(X2, into = c('x2', 'y2')) %>% 
  mutate(lines = row_number(),
         x = map2(x1, x2, seq),
         y = map2(y1, y2, seq)) %>% 
  unnest(c(x, y)) %>% 
  count(x, y) %>% 
  filter(n > 1) %>% 
  count()


