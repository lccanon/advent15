setwd("~/Documents/divers/advent15")

# https://www.reddit.com/r/adventofcode/wiki/archives/solution_megathreads/2015/

# https://www.reddit.com/r/programming/comments/3uyl7s/daily_programming_puzzles_at_advent_of_code/

library(tidyverse)

paran <- (readLines("input1") %>%
            str_split(""))[[1]]
print(sum(paran == "(") - sum(paran == ")"))
print(which(cumsum((paran == "(") * 2 - 1) == -1)[1])

# Better read

strsplit(scan("input1","character"),"")[[1]] %>%
  match(c(")","","(")) - 2
