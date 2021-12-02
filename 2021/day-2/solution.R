library(here)
library(readr)
library(dplyr)

df <- read_delim(here('2021', 'day-2', 'data.txt'),
                 col_names = c('direction', 'distance'),
                 delim = ' ')  

df %>% 
  mutate(horizontal = if_else(direction == 'forward', distance, 0),
         depth = case_when(direction == 'down' ~ distance,
                              direction == 'up' ~ -distance,
                              TRUE ~ 0)) %>% # If direction is up then make the distance negative
  summarise(horizontal = sum(horizontal, na.rm = TRUE),
            depth = sum(depth, na.rm = TRUE)) %>% # Take the sum
  summarise(answer = horizontal * depth) # Multiply

df %>%  
  mutate(aim = cumsum(depth), # Get a running total of the aim
         depth = aim * horizontal) %>% # The depth is the aim * horizontal
  summarise(answer = sum(horizontal) * sum(depth)) # Multiply

