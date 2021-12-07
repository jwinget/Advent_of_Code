library(here)
library(tidyverse)

#---- read-data ----
data_cleanup <- function(path) {
  d <- readLines(path)
  # Split the string on commas and convert to numbers
  str_split(d, pattern = ',')[[1]] %>%
    as.numeric()
}

example <- data_cleanup(here("2021", "d7_example.txt"))
input <- data_cleanup(here("2021", "d7_input.txt"))

#---- part 1 ----
fuel_cost <- function(d) {
  target_pos <- median(d)
  tibble(pos = d) %>%
    mutate(cost = case_when(
      pos >= target_pos ~ pos - target_pos,
      pos < target_pos ~ abs(pos - target_pos)
    )) %>%
    pull(cost) %>%
    sum()
}

fuel_cost(example)
fuel_cost(input)

#---- part 2 ----
# Well this is trickier

rising_cost <- function(d) {
  # I think now the cheapest is close to the mean instead
  # But it's not quite that easy...
  # Just check the integer on either side of the mean
  # and take the min
  
  target_pos <- c(ceiling(mean(d)),
                  floor(mean(d)))
  
  fuel_sum <- function(d, target_pos) {
    res <- tibble(pos = d, target_pos = target_pos) %>%
      rowwise() %>%
      mutate(cost = case_when(
        # Also need to update the cost calculation
        pos >= target_pos ~ sum(seq(from = 1,
                                  to = pos - target_pos)),
        pos < target_pos ~ sum(seq(from = 1,
                                 to = abs(pos - target_pos)))
      ))

    res %>%
      pull(cost) %>%
      sum()
  }
  
  min(unlist(map(target_pos, ~fuel_sum(d, .x))))
}

rising_cost(example)
# Should be 168

rising_cost(input)
# 98925156 too high
# 98925151 is correct

#---- follow-up ----
# Optimizations after submitting
# A comment on reddit says:
# It is possible to compute the exact value target by 
# adjusting the mean by adding
# (num_crabs - 2*(num_values_smaller_than_mean))/(2*num_crabs)
# And then rounding to the nearest integer

rising_cost_2 <- function(d) {
  # I think now the cheapest is close to the mean instead
  # But it's not quite that easy...
  # Just check the integer on either side of the mean
  # and take the min
  
  target_pos <- round(mean(d) +(length(d) - 
                          2*(length(d[d < mean(d)])))/(2*length(d)))
  
  fuel_sum <- function(d, target_pos) {
    res <- tibble(pos = d, target_pos = target_pos) %>%
      rowwise() %>%
      mutate(cost = case_when(
        # Also need to update the cost calculation
        pos >= target_pos ~ sum(seq(from = 1,
                                    to = pos - target_pos)),
        pos < target_pos ~ sum(seq(from = 1,
                                   to = abs(pos - target_pos)))
      ))
    
    res %>%
      pull(cost) %>%
      sum()
  }
  
  min(unlist(map(target_pos, ~fuel_sum(d, .x))))
}

rising_cost_2(example)
rising_cost_2(input)

# Confirming this works