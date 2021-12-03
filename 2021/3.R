library(tidyverse)
library(stringr)

#---- input ----
example <- tibble(diag = c(
  "00100",
  "11110",
  "10110",
  "10111",
  "10101",
  "01111",
  "00111",
  "11100",
  "10000",
  "11001",
  "00010",
  "01010"
))

input <- read_csv(here('2021', 'd3_input.txt'),
                  col_names = c("diag"))


#---- solution ----
output <- function(d) {
  max_len <- max(nchar(d$diag))
  
  cols <- letters[1:max_len]
  
  counts <- d %>%
    separate(diag, into = cols, sep = "(?<=.)") %>%
    pivot_longer(cols = everything()) %>%
    group_by(name, value) %>%
    summarize(n = n()) %>%
    arrange(-n) %>%
    ungroup() %>%
    group_by(name)
  
  g <- counts %>%
    slice_max(n) %>%
    select(-n) %>%
    arrange(name) %>%
    pivot_wider(names_from = name,
                values_from = value)
  
  # For some reason regular paste doesn't work
  g <- stringr::str_c(g[1,], collapse = "")
  
  e <- counts %>%
    slice_min(n) %>%
    select(-n) %>%
    arrange(name) %>%
    pivot_wider(names_from = name,
                values_from = value)
  
  # For some reason regular paste doesn't work
  e <- stringr::str_c(e[1,], collapse = "")
  
  # Convert binary strings to decimal
  gamma <- strtoi(g, base = 2)
  epsilon <- strtoi(e, base = 2)
  
  return(gamma * epsilon)
}

output(example)
output(input)
