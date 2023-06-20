library(tidyverse)

dd <- read_delim("input9", delim = " ",
                 col_names = c("src", "X1", "dst", "X2", "dist")) %>%
  select(-X1, -X2)
city <- factor(unique(c(dd$src, dd$dst)))
distances <- dd %>%
  mutate(tmp = dst) %>%
  mutate(dst = src) %>%
  mutate(src = tmp) %>%
  select(-tmp) %>%
  mutate(src = factor(src, levels(city))) %>%
  mutate(dst = factor(dst, levels(city))) %>%
  rbind(dd)

trajects <- distances %>%
  mutate(curr_dist = dist) %>%
  mutate(curr_pos = dst) %>%
  mutate(visited = 2^as.numeric(src) + 2^as.numeric(dst)) %>%
  select(-dst, -src, -dist)

# Number of cities minus 2
for (i in 3:length(city))
  trajects <- trajects %>%
    inner_join(distances, by = c("curr_pos" = "src")) %>%
    filter(bitwAnd(visited, 2^as.numeric(dst)) == 0) %>%
    mutate(curr_dist = curr_dist + dist) %>%
    mutate(curr_pos = dst) %>%
    mutate(visited = visited + 2^as.numeric(dst)) %>%
    select(-dst, -dist) %>%
    # optimization to prune inefficient solutions
    group_by(visited, curr_pos) %>%
    summarise(curr_dist = max(curr_dist), .groups = 'drop')
trajects %>%
  summarise(distance = max(curr_dist)) %>%
  print

# Visualization
library(igraph)
# dd <- dd %>% filter(dist < 100)
g <- graph_from_edgelist(as.matrix(dd[,c("src", "dst")]),
                         directed = FALSE) %>%
  set_edge_attr("label", value = dd$dist)
# plot(g, layout = layout_with_drl(g, weights = 1 / dd$dist))
# plot(g, layout = layout_with_fr(g, weights = 1 / dd$dist))
# plot(g, layout = layout_with_kk(g, weights = dd$dist))
plot(g)
