library(tidyverse)

read_csv("input24", col_names = FALSE) %>% unlist() -> weights

library(combinat)

# Find all combinations of vector x of size m with sum s
find_combn_sum <- function(x, m, s) {
  combs <- t(combn(x, m))
  combs[rowSums(combs) == s,] %>% matrix(ncol = m)
}

# Check that vector x can be divided equally in n parts
check_divisible <- function(x, n) {
  s <- sum(x) / n
  comb.range <- (1:length(x))[cumsum(x) < s & cumsum(rev(x)) > s]
  for (i in comb.range) {
    combs <- find_combn_sum(x, i, s)
    if (nrow(combs) == 0)
      next
    if (n == 2)
      return(TRUE)
    else {
      for (j in seq_len(nrow(combs)))
        if (check_divisible(x[!x %in% combs[j,]], n - 1))
          return(TRUE)
    }
  }
  return(FALSE)
}

N <- 3
s <- sum(weights) / N
comb.range <- (1:length(weights))[cumsum(weights) < s & cumsum(rev(weights)) > s]
for (i in comb.range) {
  combs <- find_combn_sum(weights, i, s)
  combs <- combs[order(apply(combs, 1, prod)),]
  for (j in seq_len(nrow(combs))) {
    x <- weights[!weights %in% combs[j,]]
    if (check_divisible(x, N - 1)) {
      print(c(i, j, prod(combs[j,])))
      stop()
    }
  }
}
