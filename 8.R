library(tidyverse)

strs <- readLines("input8")
sum(map_int(strs, str_length)) -
  strs %>%
  str_replace_all("\\\\x[0-9a-f]{2}", "X") %>%
  str_replace_all("\\\\[\"\\\\]", "Y") %>%
  str_replace_all("\"", "") %>%
  map_int(str_length) %>%
  sum
strs %>%
  str_replace_all("\\\\", "\\\\\\\\") %>%
  str_replace_all("\"", "\\\\\"") %>%
  str_replace_all("^|$", "\"") %>%
  map_int(str_length) %>%
  sum - sum(map_int(strs, str_length))
