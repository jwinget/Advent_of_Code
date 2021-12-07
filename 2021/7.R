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
  # Need to figure out if there are more entries to one side of the mean
  
  if (length(d[d > mean(d)]) < length(d[d < mean(d)])) {
    # More on the high end, use ceiling
    target_pos <- ceiling(mean(d))
  } else {
    # More on the lower end, use floor
    target_pos <- floor(mean(d))
  }
  
  print(target_pos)

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

rising_cost(example)
# Should be 168

rising_cost(input)
# 98925156 too high
# 98925151 is correct