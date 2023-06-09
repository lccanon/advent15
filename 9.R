library(tidyverse)

dd <- read_delim("input9", delim = " ",
                 col_names = c("src", "X1", "dst", "X2", "dist")) %>%
  select(-X1, -X2)
city <- factor(unique(c(dd$src, dd$dst)))
dd <- dd %>%
  mutate(tmp = dst) %>%
  mutate(dst = src) %>%
  mutate(src = tmp) %>%
  select(-tmp) %>%
  mutate(src = factor(src, city)) %>%
  mutate(dst = factor(dst, city)) %>%
  rbind(dd)

trajects <- dd %>%
  mutate(curr_dist = dist) %>%
  mutate(curr_pos = dst) %>%
  mutate(visited = 2^as.numeric(src) + 2^as.numeric(dst)) %>%
  select(-dst, -src, -dist)

# Number of cities minus 2
for (i in 3:length(city))
  trajects <- trajects %>%
    inner_join(dd, by = c("curr_pos" = "src")) %>%
    filter(bitwAnd(visited, 2^as.numeric(dst)) == 0) %>%
    mutate(curr_dist = curr_dist + dist) %>%
    mutate(curr_pos = dst) %>%
    mutate(visited = visited + 2^as.numeric(dst)) %>%
    select(-dst, -dist) %>%
    group_by(visited, curr_pos) %>%
    arrange(desc(curr_dist)) %>%
    slice(1) %>%
    ungroup
print(trajects %>%
  arrange(desc(curr_dist)) %>%
  slice(1) %>%
  select(curr_dist))
