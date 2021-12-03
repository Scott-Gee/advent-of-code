library(here)
library(dplyr)
library(purrr)
library(base)
library(glue)

df <- read_table(here('2021', 'day-3', 'data.txt'), col_names = FALSE) 

# Part 1

## All observations have 12 characters

common_value <- function(n, type = c('most', 'least')){
  x <- as.numeric(substring(df$X1, n, n))  # Take the nth character 
  
  unique_x <- unique(x) # Get uniques (0,1)
  
  if(type == 'most'){
  unique_x[which.max(tabulate(match(x, unique_x))]  # Get the list of 0s and 1s and take the max
  } else if(type == 'least'){
    unique_x[which.min(tabulate(match(x, unique_x))] # Get the list of 0s and 1s and take the min
  }
}

most <- map_dbl(1:12, ~ common_value(.x, type = 'most')) %>% # For characters 1:12 get the most common value
  glue_collapse() %>% # Collapse in to single string
  strtoi(., base = 2)  # Convert to decimal

least <- map_dbl(1:12, ~ common_value(.x, type = 'least')) %>% 
  glue_collapse() %>% 
  strtoi(., base = 2)


most * least


## Part 2

## For loop this time, can't find a way to update a dataframe based on results in tidyverse

# Most common
oxygen <- df$X1

for(i in 1:12){ # Loop through 12 characters
  
  x <- as.numeric(substring(oxygen, i, i))
  
  unique_x <- unique(x)
  
  common <- unique_x[which.max(tabulate(match(x, unique_x)) + c(0, 1))]  # The c(0,1) adds one on to the value 1 as if they are equally common you take 1
  
  oxygen <- oxygen[x == common]   # Only return those that are TRUE and pass through to new iteration
  }

# Least common
co2 <- df$X1

for(i in 1:12){ # Loop through 12 characters
  
  x <- as.numeric(substring(co2, i, i))
  
  unique_x <- unique(x)
  
  common <- unique_x[which.min(tabulate(match(x, unique_x)) + c(1, 0))] # The c(1,0) adds one on to the value 0 as if they are equally common you take 0
  
  co2 <- co2[x == common]   # Only return those that are TRUE and pass through to new iteration
}

strtoi(oxygen, base = 2) * strtoi(co2, base = 2)




  
