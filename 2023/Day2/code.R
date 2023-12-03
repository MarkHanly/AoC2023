#AoC 2023 Day 2

library(dplyr)
library(stringr)

test <- here::here('2023/Day2/test.txt')
input <- here::here('2023/Day2/input.txt')

checkBlue <- function(str){
  str_extract(str, pattern = "\\d+(?= blue)")
}

checkRed <- function(str){
  str_extract(str, pattern = "\\d+(?= red)")
}

checkGreen <- function(str){
  str_extract(str, pattern = "\\d+(?= green)")
}

myMax <- function(list){
  max(as.numeric(list), na.rm = TRUE)
}

valid <- c(12, 13, 14)

read.delim(input, header = FALSE, col.names = 'code') |>
  mutate(
    id = row_number(),
    length = stri_length(code),
    red = lapply(lapply(str_split(code, ';'), checkRed), myMax),
    green = lapply(lapply(str_split(code, ';'), checkGreen), myMax),
    blue = lapply(lapply(str_split(code, ';'), checkBlue), myMax),
    valid = ifelse(red<=valid[1] & green<=valid[2] & blue <= valid[3] , 1, 0)
    ) |>
  filter(valid==1) |>
  summarise(ans = sum(id))


# Part II

read.delim(input, header = FALSE, col.names = 'code') |>
  mutate(
    id = row_number(),
    length = stri_length(code),
    red = as.numeric(lapply(lapply(str_split(code, ';'), checkRed), myMax)),
    green = lapply(lapply(str_split(code, ';'), checkGreen), myMax),
    blue = lapply(lapply(str_split(code, ';'), checkBlue), myMax),
    power = as.numeric(red)*as.numeric(blue)*as.numeric(green)
    ) |>
  summarise(ans = sum(power))

### # JAGO # ###
