library(tidyverse)
library(digest)

key <- "bgvyzdsv"
# key <- "abcdef"
leading_zero <- 6

i <- 1
repeat {
  md5 <- digest(str_c(key, i), algo = "md5", serialize = FALSE)
  if (str_sub(md5, 0, leading_zero) == str_flatten(rep("0", leading_zero)))
    break
  i <- i + 1
}
print(i)
