Day <- 1

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("data", glue("day_{Day}_input.txt"))) %>%
  as.numeric()

# Functions ---------------------------------------------------------------
find_expenses <- function(d, n) {
  res <- combn(d, n)[, which(colSums(combn(d, n)) == 2020)]
  return(prod(res))
}

# Question 1 --------------------------------------------------------------
answer1 <- find_expenses(d, 2)
answer1

# Question 2 --------------------------------------------------------------
answer2 <- find_expenses(d, 3)
answer2
