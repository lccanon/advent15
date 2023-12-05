library(tidyverse)

read_lines("input01") %>%
  str_split_1("") -> paran
print(sum(paran == "(") - sum(paran == ")"))
print(which(cumsum(match(paran, c(")", "", "(")) - 2) == -1)[1])

# Better read

strsplit(scan("input01", "character"), "")[[1]] %>%
  match(c(")", "", "(")) - 2
