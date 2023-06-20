number <- 36000000

library(numbers)

max_present <- 0
for (i in 1:1000000) {
  ## seq_i <- 1:i
  ## present <- sum(seq_i[i %% seq_i == 0] * 10)
  ## present <- sum(seq_i[i %% seq_i == 0 & i %/% seq_i <= 50] * 11)
  # present <- Sigma(i) * 10
  div <- divisors(i)
  present <- sum(div[div * 50 >= i]) * 11
  max_present <- max(max_present, present)
  if (i %% 1000 == 0)
    print(c(i, present, max_present))
  if (present >= number)
    break
}
print(i)

# Actual solution (run in reasonable time, O(nlog(n)), instead of
# O(n^2) above)

house <- rep(0, number)
for (i in 1:number) {
  # idx <- seq(from = i, to = number, by = i)
  idx <- seq(from = i, by = i, length.out = 50)
  house[idx] <- house[idx] + i
  if (11 * house[i] >= number)
    break
}
print(i)
