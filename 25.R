library(tidyverse)
library(gmp)

microbenchmark::microbenchmark({

readLines("input25") %>% str_extract_all("\\d+") %>% unlist %>% as.integer -> inst
row <- inst[1]
col <- inst[2]

rank <- (col + row - 1) * (col + row - 2) / 2 + col
code <- as.bigz(20151125, 33554393) * as.bigz(252533, 33554393) ^ (rank - 1)
print(as.character(code))

})
