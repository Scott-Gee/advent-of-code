library(dplyr)
library(here)

numbers <- read.table(here('2021', 'day-4', 'data.txt'), nrows = 1, sep = ',')

boards <- read.table(here('2021', 'day-4', 'data.txt'), skip = 2)

completed <- boards %>% 
  mutate(board = ceiling(row_number()/5)) %>% # get the board number by dividing the rows by 5 and roudning up
  group_by(board) %>% 
  mutate(row = row_number()) %>%  # get the spdcific row of nthe value for each board
  ungroup() %>% 
  pivot_longer(-c(board, row), names_to = 'column') %>% # Pivot data from wide to long
  mutate(column = as.integer(str_remove_all(column, 'V')),
         turn = match(value, numbers)) %>% # Match will return the index of where data is matched e.g. a list of 1,2,3 compared to 4, 4, 1 would retunr a match of 3 for the value 1 
  group_by(board, row) %>% 
  mutate(row_complete = max(turn)) %>% # For the rows, find the last index where it is matched (a complete row)
  ungroup() %>% 
  group_by(board, column) %>% 
  mutate(col_complete = max(turn)) # For the columns, find the last index where it is matched (a complete column)

## Part 1
completed %>% 
  ungroup() %>% 
  group_by(board) %>% 
  summarise(complete = min(row_complete, col_complete)) %>% # For each board, get the minimum of the rows and columns (when we first get a line)
  filter(complete == min(complete)) %>% # Filter for only the lowest match (the first board to get a line)
  inner_join(completed) %>% # Join back to get the answer
  filter(turn > complete) %>% # Keep only the rows that haven't been matched 
  summarise(sum(value)) %>% 
  pull() * 32 # Multiply by the value that was last pulled out


## Part 2
completed %>% 
  ungroup() %>% 
  group_by(board) %>% 
  summarise(complete = min(row_complete, col_complete)) %>% 
  filter(complete == max(complete)) %>% # Same as above but take the max
  inner_join(completed) %>%
  filter(turn > complete) %>% 
  summarise(sum(value)) %>% 
  pull() * 82
