library(tidyverse)

L <- 150
liters <- read_delim("input17", delim = " ",
                    col_names = "liter")

comb <- array(0, dim = c(nrow(liters), L, nrow(liters)))
lit <- liters[1,]$liter
comb[1,lit,1] <- 1
for (j in 1:L)
  for (i in 2:nrow(liters))
    for (k in 1:nrow(liters)) {
      lit <- liters[i,]$liter
      # Basic case : we do not take element i
      comb[i,j,k] <- comb[i - 1,j,k]
      if (j - lit == 0 && k == 1)
        # Case when element i fit exactly
        comb[i,j,k] <- comb[i,j,k] + 1
      else if (j - lit > 0 && k > 1)
        # Now, we consider we take element i but it does not fit
        comb[i,j,k] <- comb[i,j,k] + comb[i - 1,j - lit, k - 1]
    }
print(sum(comb[nrow(liters),L,]))
print(comb[nrow(liters),L,which(comb[nrow(liters),L,] != 0)[1]])

# Solution based on combinat
# https://www.reddit.com/r/adventofcode/comments/3x6cyr/comment/cy27q72

library(combinat)

total.volume <- 150
combinations <- 0
x <- sort(as.integer(readLines("input17")))
comb.range <- (1:length(x))[cumsum(x) < total.volume & cumsum(rev(x)) > total.volume]

for (i in comb.range){
  combinations <- combinations + sum(rowSums(t(combn(x,i))) == total.volume)
  print(c(i, combinations))
}
