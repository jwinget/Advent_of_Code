library(here)
library(tidyverse)

#---- part 1 ----
parse_input <- function(path, board_width = 5) {
  # Handle the draws
  draw_str <- readLines(path)[[1]]
  # Split on the comma and convert to numbers
  draws <- as.numeric(unlist(strsplit(draw_str, ",")))
  
  # Handle the boards
  board_strings <- readLines(path)[-1]
  
  # Get rid of the empty rows
  board_strings <- board_strings[!board_strings == ""]
  
  # Collapse the boards into a single string
  board_string <- paste(board_strings, collapse = " ")
  
  # Split the string on whitespace and convert to numbers
  board_vals <- str_split(board_string, "\\s+")[[1]] %>%
    as.numeric()
  
  # Convert to tidy data frame with board, row, and column indexes
  boards <- tibble(board = rep(seq(from = 1, to = length(board_vals) / board_width^2), each = board_width^2),
                   row = rep(1:5, each = 5, times = length(board_vals) / board_width^2),
                   column = rep(1:5, times = length(board_vals) / board_width),
                   value = board_vals)
  
  return(list(draws = draws,
              boards = boards))
}

input <- parse_input(here("2021", "d4_input.txt"))

head(input$boards)
tail(input$boards)

find_winner <- function(draws, boards, n = -1) {
  # Find the turn each value will be drawn on
  draw_df <- tibble(value = draws) %>%
    rownames_to_column(var = "draw_turn") %>%
    mutate(draw_turn = as.numeric(draw_turn))
  
  # Now find the winning board
  winning_board <- boards %>%
    left_join(draw_df) %>%
    group_by(board, row) %>%
    mutate(row_win = max(draw_turn)) %>%
    ungroup() %>%
    group_by(board, column) %>%
    mutate(col_win = max(draw_turn)) %>%
    ungroup() %>%
    group_by(board) %>%
    mutate(win_turn = min(row_win, col_win)) %>%
    ungroup() %>%
    #select(-row_win, -col_win, -row, -column, -board) %>%
    top_n(n, wt = win_turn)
  
  View(winning_board)
  
  val_sum <- winning_board %>%
    # Determine unmarked numbers
    mutate(marked = case_when(
      draw_turn <= win_turn ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    # Get sum of unmarked numbers
    filter(marked == FALSE) %>%
    summarise(val_sum = sum(value)) %>%
    pull()
  
  # Multiply val_sum by last pulled number
  val_sum * draws[first(winning_board$win_turn)]
}

find_winner(input$draws, input$boards)
# 8136 is correct

#---- part 2 ----
find_winner(input$draws, input$boards, n = 1)
