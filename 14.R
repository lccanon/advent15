library(tidyverse)

beast <- read_delim("input14", delim = " ",
                    col_types = "c--i--i------i-",
                    col_names = c("name", NULL, NULL, "speed", NULL, NULL,
                                  "fly", rep(NULL, 6), "rest", NULL))

score <- rep(0, nrow(beast))
for (duration in 1:2503) {
  progress <- (duration %/% (beast$fly + beast$rest) * beast$fly +
                 pmin(duration %% (beast$fly + beast$rest), beast$fly)) * beast$speed
  best <- progress == max(progress)
  score[best] <- score[best] + 1
}
max(score)

# Other parsing
beast <- readLines("input14") %>%
  str_replace("^([^ ]*).* (\\d+) .* (\\d+) .* (\\d+).*$", "\\1,\\2,\\3,\\4") %>%
  tibble %>%
  separate(col = ".", into = c("name", "speed", "fly", "rest"), sep = ",") %>%
  mutate_at(2:4, as.integer)
