library(tidyverse)

strs <- readLines("input5")

print(sum(str_detect(strs, regex("[aeiou].*[aeiou].*[aeiou]")) &
            !str_detect(strs, regex("ab|cd|pq|xy")) &
            str_detect(strs, regex("(.)\\1"))))
print(sum(str_detect(strs, regex("(.{2}).*\\1")) &
            str_detect(strs, regex("(.).\\1"))))
