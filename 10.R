library(tidyverse)

chain <- str_split("1113222113", "")[[1]]

for (i in 1:50) {
  chain <- (do.call(rbind, rle(chain)) %>%
              as.vector %>%
              str_flatten %>%
              str_split(""))[[1]]
  print(c(i, length(chain), max(chain)))
}
