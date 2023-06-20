library(numbers)

max_present <- 0
for (i in 1:1000000) {
  seq_i <- 1:i
  # present <- sum(seq_i[i %% seq_i == 0] * 10)
  # present <- sum(seq_i[i %% seq_i == 0 & i %/% seq_i <= 50] * 11)
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
