library(tidyverse)

bitwNot <- function(X) {
  2^16 - 1 - X
}

bitwShiftL <- function(X, Y) {
  base::bitwShiftL(X, Y) %% 2^16
}

insts <- readLines("input7") %>%
  str_to_upper() %>%
  str_replace(regex("(.*) -> (.*)"), regex("\\2 <- \\1")) %>%
  str_replace("<- (.*) OR (.*)", "<- bitwOr(\\1, \\2)") %>%
  str_replace("<- (.*) AND (.*)", "<- bitwAnd(\\1, \\2)") %>%
  str_replace(" NOT (.*)", " bitwNot(\\1)") %>%
  str_replace("<- (.*) ([RL])SHIFT (.*)", "<- bitwShift\\2(\\1, \\3) ") %>%
  str_replace("^B .*", "B <- 46065")
while(!"A" %in% ls()) {
  print(length(ls()))
  for (inst in insts)
    try(eval(parse(text = inst)))
}
print(A)

# Verif
for (var in as.list(.GlobalEnv))
  if (is.numeric(var) && (var < 0 || var >= 2^16)) print(var)

