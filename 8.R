library(tidyverse)

strs <- readLines("input8")
sum(map_int(strs, str_length)) -
  strs %>%
  str_replace_all(regex("\\\\x[0-9a-f]{2}"), "X") %>%
  str_replace_all(regex("\\\\[\"\\\\]"), "Y") %>%
  str_replace_all(regex("\""), "") %>%
  map_int(str_length) %>%
  sum
strs %>%
  str_replace_all(regex("\\\\"), "\\\\\\\\") %>%
  str_replace_all(regex("\""), "\\\\\"") %>%
  str_replace_all(regex("^|$"), "\"") %>%
  map_int(str_length) %>%
  sum - sum(map_int(strs, str_length))
