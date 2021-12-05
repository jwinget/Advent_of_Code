library(here)
library(tidyverse)

parse_input <- function(path) {
  lines <- readLines(path)
  
  # Convert to a tibble
  vecs <- tibble(raw = lines) %>%
    # Separate on arrow thing
    separate(raw, 
             into = c("start_coord", "end_coord"), 
             sep = " -> ") %>%
    # Now split those two
    separate(start_coord, 
             into = c("x1", "y1"),
             sep = ",") %>%
    separate(end_coord,
             into = c("x2", "y2"),
             sep = ",") %>%
    # Make them numbers
    mutate_all(as.numeric) %>%
    # Keep the vent line ID in case we need it?
    rownames_to_column(var = "vent_line")
  
  return(vecs)
}

example <- parse_input(here("2021", "d5_example.txt"))
input <- parse_input(here("2021", "d5_input.txt"))

find_danger <- function(vecs, diag = FALSE) {
  danger_points <- vecs %>%
    group_by(vent_line) %>%
    # Just keep vertical or horizontal lines
    filter(if (diag == FALSE) x1 == x2 | y1 == y2 else TRUE) %>%
    # Compute all x & y points each line passes through
    mutate(x_coords = list(seq(from = x1, to = x2)),
           y_coords = list(seq(from = y1, to = y2))) %>%
    select(vent_line, x_coords, y_coords) %>%
    unnest(cols = c(x_coords, y_coords), keep_empty = TRUE) %>%
    # Count overlaps
    mutate(xy = paste(x_coords, y_coords)) %>%
    ungroup() %>%
    group_by(xy) %>%
    summarize(n = n()) %>%
    filter(n >= 2)
  
  return(nrow(danger_points))
}

find_danger(example)

find_danger(input)
# 4993, right on the first try :)

#---- part 2 ----
# Just set diag = TRUE
find_danger(example, diag = TRUE)

find_danger(input, diag = TRUE)
# 21101 is correct.