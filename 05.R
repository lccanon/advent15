library(tidyverse)

strs <- readLines("input01")

print(sum(str_detect(strs, "[aeiou].*[aeiou].*[aeiou]") &
            !str_detect(strs, "ab|cd|pq|xy") &
            str_detect(strs, "(.)\\1")))
print(sum(str_detect(strs, "(.{2}).*\\1") &
            str_detect(strs, "(.).\\1")))
