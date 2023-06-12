library(tidyverse)

insts <- readLines("input7") %>%
  str_replace(".* b$", "46065 -> b") %>%
  str_replace_all("([a-z]{1,2}) ", "as.numeric(vars[[\"\\1\"]]) ") %>%
  str_replace("([a-z]{1,2})$", "vars[[\"\\1\"]]") %>%
  str_replace("NOT (.*) ->", "bitwNot(\\1) ->") %>%
  str_replace("(.*) OR (.*) ->", "bitwOr(\\1, \\2) ->") %>%
  str_replace("(.*) AND (.*) ->", "bitwAnd(\\1, \\2) ->") %>%
  str_replace("(.*) ([RL])SHIFT (.*) ->", "bitwShift\\2(\\1, \\3) ->")

vars <- list()
while(length(vars[["a"]]) == 0) {
  print(sum(map_int(vars, length)))
  for (inst in insts)
    eval(parse(text = inst))
}
print(vars[["a"]] %% 2^16)
