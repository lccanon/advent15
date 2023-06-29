library(tidyverse)

read_file("input01") %>%
  str_split("", simplify = TRUE) -> paran
print(sum(paran == "(") - sum(paran == ")"))
print(which(cumsum(match(paran, c(")", "", "(")) - 2) == -1)[1])

# Better read

strsplit(scan("input01", "character"), "")[[1]] %>%
  match(c(")", "", "(")) - 2
