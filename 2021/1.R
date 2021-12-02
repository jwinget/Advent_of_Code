library(here)
library(tidyverse)

#---- Part One ----
examples <- c(
  199,
  200,
  208,
  210,
  200,
  207,
  240,
  269,
  260,
  263
)

#---- prototyping ----
imap(examples, ~.x - examples[.y-1] > 0) %>%
  unlist() %>%
  sum()

#---- actual input ----
input <- scan("2021/d1_input.txt")

imap(input, ~.x - input[.y-1] > 0) %>%
  unlist() %>%
  sum()
# 1754 is correct

#---- Part Two ----
window_sums <- imap(examples, ~.x + examples[.y+1] + examples[.y+2]) %>%
  unlist()

imap(window_sums, ~.x - window_sums[.y-1] > 0) %>%
  unlist() %>%
  sum()

window_sums <- imap(input, ~.x + input[.y+1] + input[.y+2]) %>%
  unlist()

imap(window_sums, ~.x - window_sums[.y-1] > 0) %>%
  unlist() %>%
  sum(na.rm = T)

# 1771 too low
# 1789 is correct

# A better solution from Discord
# Part 1 
sum(diff(input) > 0)

# Part 2
input_roll <- zoo::rollsum(input, 3)

sum(diff(input_roll) > 0)