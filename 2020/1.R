library(tidyverse)

examples <- c(1721,
              979,
              366,
              299,
              675,
              1456)

# Find products of n that sum to 2020
find_prods <- function(values, n) {
  target <- 2020
  s <- FALSE
  while(!s) {
    candidates <- sample(values, n)
    if(sum(candidates) == target) {
      return(prod(candidates))
    }
  }
}

# Test examples
find_prods(examples, 2)
# 514579

# Run on actual input
values <- scan(here('2020', '1_input.txt'))

# Part 1
find_prods(values, 2)
# 926464

# Part 2
find_prods(values, 3)
# 65656536