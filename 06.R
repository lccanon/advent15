library(tidyverse)

insts <- readLines("input6") %>%
  str_replace("turn ", "") %>%
  str_replace(" through ", ",") %>%
  str_replace(" ", ",") %>%
  tibble() %>%
  separate(sep = ",", col = ".", into = c("action", "x1", "y1", "x2", "y2"))

n <- 1000
lights <- matrix(0, nrow = n, ncol = n)
for (i in 1:nrow(insts)) {
  index_i <- pull(insts[i,], x1):pull(insts[i,], x2) + 1
  index_j <- pull(insts[i,], y1):pull(insts[i,], y2) + 1
  if (insts[i,"action"] == "on")
    lights[index_i,index_j] <- lights[index_i,index_j] + 1
  else if (insts[i,"action"] == "off")
    lights[index_i,index_j] <- lights[index_i,index_j] - 1
  else if (insts[i,"action"] == "toggle")
    lights[index_i,index_j] <- lights[index_i,index_j] + 2
  else
    print("Problem")
  lights[lights < 0] <- 0
}
print(sum(lights))
