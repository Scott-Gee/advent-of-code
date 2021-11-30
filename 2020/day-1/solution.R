library(dplyr)
library(tidyr)
library(here)

source(here('2020', 'day-1', 'data.R'))

tibble(value = list) %>% 
  expand(value, 
         value1 = value) %>% 
  filter(value < value1) %>% 
  mutate(sum = value + value1) %>% 
  filter(sum == 2020) %>% 
  mutate(product = value * value1) %>% 
  pull(product)

tibble(value = list) %>% 
  expand(value,
         value1 = value,
         value2 = value) %>% 
  filter(value < value1,
         value < value2,
         value1 < value2) %>% 
  mutate(sum = value + value1 + value2) %>% 
  filter(sum == 2020) %>% 
  mutate(product = value * value1 * value2) %>% 
  pull(product)


