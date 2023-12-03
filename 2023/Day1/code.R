#AoC 2023 Day 1

library(dplyr)
library(stringr)

test <- here::here('2023/Day1/test.txt')
input <- here::here('2023/Day1/input.txt')

data <- input

read.delim(data, header = FALSE, col.names = 'code') |>
  mutate(
    num = gsub("[a-z]+", "", code),
    first = substr(num, 1, 1),
    last = substr(stringi::stri_reverse(num), 1, 1),
    sum = as.numeric(stri_c(first,last))
    ) |>
  summarise(ans = sum(sum))


## Part II

test2 <- here::here('2023/Day1/test2.txt')

read.delim(data, header = FALSE, col.names = 'code') |>
  mutate(
    code2 = gsub(c("one"), '1', code),
    code2 = gsub(c("tw"), '2', code2),
    code2 = gsub(c("thr"), '3', code2),
    code2 = gsub(c("four"), '4', code2),
    code2 = gsub(c("fiv"), '5', code2),
    code2 = gsub(c("six"), '6', code2),
    code2 = gsub(c("seven"), '7', code2),
    code2 = gsub(c("igh"), '8', code2),
    code2 = gsub(c("nin"), '9', code2),
    num = gsub("[a-z]+", "", code2),
    first = substr(num, 1, 1),
    last = substr(stringi::stri_reverse(num), 1, 1),
    sum = as.numeric(stri_c(first,last))
  ) |>
  summarise(ans = sum(sum))

