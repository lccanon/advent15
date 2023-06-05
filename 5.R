library(tidyverse)

strs <- readLines("input5")

nices <- 0
for (i in 1:length(strs)) {
  st <- strs[[i]]
  voyel <- length(str_extract_all(st, regex("[aeiou]"))[[1]])
  if (!is.na(str_extract(st, "ab|cd|pq|xy")))
    voyel <- -1
  if (voyel >= 3 && !is.na(str_extract(st, regex("(.)\\1"))))
    nices <- nices + 1
}
print(nices)

nices <- 0
for (i in 1:length(strs)) {
  st <- strs[[i]]
  if (!is.na(str_extract(st, regex("(.)(.).*\\1\\2"))) &&
      !is.na(str_extract(st, regex("(.).\\1"))))
    nices <- nices + 1
}
print(nices)

# Better version
print(sum(str_detect(strs, regex("[aeiou].*[aeiou].*[aeiou]")) &
            !str_detect(strs, regex("ab|cd|pq|xy")) &
            str_detect(strs, regex("(.)\\1"))))
print(sum(str_detect(strs, regex("(.)(.).*\\1\\2")) &
            str_detect(strs, regex("(.).\\1"))))
