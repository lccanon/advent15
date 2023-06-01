library(tidyverse)

paran <- (readLines("input1") %>%
            str_split(., ""))[[1]]
print(sum(paran == "(") - sum(paran == ")"))
print(which(cumsum((paran == "(") * 2 - 1) == -1)[1])
