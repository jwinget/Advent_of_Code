library(here)
library(tidyverse)

#---- read-data ----
example <- readLines(here("2021", "d6_example.txt"))
input <- readLines(here("2021", "d6_input.txt"))

init_data <- function(d) {
  # Split the string on commas and convert to numbers
  str_split(d, pattern = ',')[[1]] %>%
    as.numeric()
}

breed_fish <- function(init_data, days) {
  # create a table to hold counts of fish at a given timer
  age_counts <- tibble(ages = 0:8)
  
  # initialize with starting counts
  count_start <- tibble(ages = init_data) %>%
    group_by(ages) %>%
    tally()
  
  population <- age_counts %>%
    full_join(count_start, copy = TRUE) %>%
    replace(is.na(.), 0) %>%
    mutate(n = as.numeric(n))

  # Rules to update the table
  update_pop <- function(pop_table) {
    # Store the number of fish with a zero timer
    to_breed <- pop_table[[1,2]]
    
    # Shift all the timers down 1 day
    # Then update numbers at position 6 and 8
    pop_table %>%
      mutate(n = lead(n)) %>%
      replace(is.na(.), 0) %>%
      mutate(n = case_when(
               ages %in% c(6,8) ~ n + to_breed,
               TRUE ~ n
             ))
  }
  
  for (i in 1:days) {
    population <- update_pop(population)
  }
  
  return(sum(population$n))
}

#---- part 1 ----
ie <- init_data(example)
breed_fish(ie, 80)

ii <- init_data(input)
breed_fish(ii, 80)

system.time(breed_fish(ii, 256) %>%
  print(digits = 12))
# 1.28 seconds elapsed :)
