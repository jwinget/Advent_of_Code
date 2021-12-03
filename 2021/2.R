library(tidyverse)

#---- testing part 1 ----
example <- tribble(~direction, ~amount,
"forward", 5,
"down", 5,
"forward", 8,
"up", 3,
"down", 8,
"forward", 2
)

get_pos <- function(course) {
  res <- course %>%
    group_by(direction) %>%
    summarize(sum_moves = sum(amount)) %>%
    pull(sum_moves, direction)
  
  depth <- res["down"] - res["up"]
  print(res["forward"]*depth)
}

get_pos(example)
# 150

#---- solving part 1 ----
input <- read_delim(here('2021', 'd2_input.txt'),
                    col_names = c("direction", "amount"))

get_pos(input)
# 1727835 is correct

#---- testing part 2 ----
aim_pos <- function(course) {
  # Forward position is easy
  forward <- sum(filter(course, direction == "forward")$amount)
  
  # Handling depth
  d <- course %>%
    mutate(
      aim = 0,
      aim = case_when(
        direction == "down" ~ aim + amount,
        direction == "up" ~ aim - amount,
        TRUE ~ cumsum(aim)
      ),
      aim = cumsum(aim)) %>%
    filter(direction == "forward") %>%
    mutate(depth = aim * amount)
  
  depth <- sum(d$depth)
  return(forward * depth)
}

aim_pos(example)

#---- solving part 2 ----
aim_pos(input)
# 1544000595 is correct