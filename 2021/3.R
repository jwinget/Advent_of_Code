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


#---- part 1 ----
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


#---- part 2 ----
check_bit <- function(vec, flip = FALSE, position = 1) {
  even <- FALSE
  if (sum(as.numeric(vec)) > length(vec)/2) {
    # More ones
    res <- 1
  } else if (sum(as.numeric(vec)) < length(vec)/2) {
    # More zeros
    res <- 0
  } else {
    # Count is even, return the positional value
    res <- 1
  }
  if (flip) {return(as.numeric(!res))}
  return(res)
}
split_data <- function(d) {
  max_len <- max(nchar(d$diag))
  cols <- letters[1:max_len]
  return(d %>%
    separate(diag, into = cols, sep = "(?<=.)"))
}

get_readout <- function(split_d, flip) {
  d <- split_d
  res <- c()
  j <- 0
  for(i in 1:ncol(split_d)) {
    # Determine which bit to keep
    check <- check_bit(d[[letters[i]]], flip = flip, position = i-j)
    j <- j+1
    # Append it to the result
    res <- c(res, check)
    
    d <- d %>%
      filter(.data[[letters[i]]] == check)
    
    if (nrow(d) == 1) {
      # Solution is found, append the last value and return it
      print(res)
      print(j)
      print(d)
      if(ncol(d) > 1) {
        
        d <- d %>%
          select(-.data[[letters[i]]])
        bstr <- c(res, d[1,])
      }
      else {
        bstr <- res
      }
      binary <- stringr::str_c(bstr, collapse = "")
      return(strtoi(binary, base = 2))
    } else {
      # Drop the first bit
      d <- d %>%
        select(-.data[[letters[i]]])
    }
  }
}

d <- split_data(input)
ogr <- get_readout(d, flip = FALSE)
csr <- get_readout(d, flip = TRUE)

ogr*csr
