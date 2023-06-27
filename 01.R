library(tidyverse)

paran <- (readLines("input01") %>%
            str_split(""))[[1]]
print(sum(paran == "(") - sum(paran == ")"))
print(which(cumsum((paran == "(") * 2 - 1) == -1)[1])

# Better read

strsplit(scan("input01", "character"), "")[[1]] %>%
  match(c(")", "", "(")) - 2
