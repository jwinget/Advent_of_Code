library(here)
library(tidyverse)

#---- data-parsing ----
ex_raw <- readLines(here("2021", "d8_example.txt"))
in_raw <- readLines(here("2021", "d8_input.txt"))

parse_data <- function(raw_lines) {
  # Split signals from output
  dsplit <- str_split(raw_lines, " \\| ", simplify = TRUE)
  raw_signals <- dsplit[,1]
  raw_output <- dsplit[,2]

  return(raw_output)
}

example <- parse_data(ex_raw)
input <- parse_data(in_raw)

#---- part-1 ----
digit_segments = tibble(digit = 0:9,
                        segments = c("abcefg",
                                     "cf",
                                     "acdeg",
                                     "acdfg",
                                     "bcdf",
                                     "abdfg",
                                     "abdefg",
                                     "acf",
                                     "abcdefg",
                                     "abcdfg")) %>%
  mutate(segment_count = nchar(segments))

# Just get it done, worry about part 2 later
tibble(output = str_split(paste(input, 
                                collapse = " "), 
                          "\\s")[[1]]) %>%
  mutate(segment_count = nchar(output)) %>%
  left_join(digit_segments, by = "segment_count") %>%
  filter(digit %in% c(1,4,7,8)) %>%
  summarize(n = n())
