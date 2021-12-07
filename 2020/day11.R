Day <- 11

# This runs obscenely slowly but it's *my* code, at least

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("2020", "data", glue("day_{Day}_input.txt")))
dt <- c("L.LL.LL.LL", "LLLLLLL.LL", "L.L.L..L..", "LLLL.LL.LL",
        "L.LL.LL.LL", "L.LLLLL.LL", "..L.L.....", "LLLLLLLLLL",
        "L.LLLLLL.L", "L.LLLLL.LL")

# Functions ---------------------------------------------------------------

# Da Rulez
# All decisions are based on the number of occupied seats adjacent
# to a given seat (one of the eight positions immediately up,
# down, left, right, or diagonal from the seat)
# If a seat is empty (L) and there are no occupied seats adjacent to it,
# the seat becomes occupied.
# If a seat is occupied (#) and four or more seats adjacent
# to it are also occupied, the seat becomes empty.
# Otherwise, the seat's state does not change.

seat_grid <- function(d) {
  # Encode the seating chart locations
  tibble(type  = str_split(str_c(d, collapse = ""), pattern = "")[[1]],
                     row = rep(1:length(d), each = nchar(d)[1]),
                     column = rep(1:nchar(d)[1],
                                  times = length(d))) %>%
    rownames_to_column(var = "seat_number")
}

neighboring_seats <- function(seat_map, remove_floor = FALSE) {
  # Find neighboring/visible seats. Use remove_floor = TRUE for part 2
}

update_seats <- function(occupied_map, n = 4) {
  # Process the rules and return a new occupied map
  # Use n = 5 for part 2
}

fill_ferry <- function(seat_map, remove_floor = FALSE, n = 4) {
  # Run seat update until stable and return number of occupied seats
}

# Question 1 --------------------------------------------------------------
nb <- find_neighbors(d, mode = "adjacent")
answer1 <- update_seats(nb, 4)
answer1
# 2166 is correct

# Question 2 --------------------------------------------------------------
nb <- find_neighbors(d, mode = "visible")
answer2 <- update_seats(nb, 5)
answer2