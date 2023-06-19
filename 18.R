library(tidyverse)

readLines("input18") %>%
  str_split("") %>%
  do.call(what = rbind) -> lights

lights[lights == "#"] <- TRUE
lights[lights == "."] <- FALSE
lights <- matrix(as.logical(lights), ncol = ncol(lights))

lights <- cbind(FALSE, lights, FALSE)
lights <- rbind(FALSE, lights, FALSE)

lights.next <- lights
for (k in 1:100) {
  for (i in 2:(nrow(lights) - 1))
    for (j in 2:(ncol(lights) - 1)) {
      if (i == 2 && j == 2 || i == nrow(lights) - 1 && j == 2 ||
          i == 2 && j == ncol(lights) - 1 ||
          i == nrow(lights) - 1 && j == ncol(lights) - 1)
        next
      neigh_on <- sum(lights[(i - 1):(i + 1), (j - 1):(j + 1)])
      if (lights[i,j])
        lights.next[i,j] <- neigh_on %in% (2:3 + 1)
      else
        lights.next[i,j] <- neigh_on == 3
    }
  lights <- lights.next
  print(sum(lights))
}

# Second solution (inefficient)

readLines("input18") %>%
  str_split("") %>%
  do.call(what = rbind) -> lights

as_tibble(lights == "#") %>%
  rowid_to_column() %>%
  pivot_longer(-rowid, names_to = "colid", values_to = "on") %>%
  mutate(colid = as.integer(str_extract(colid, "\\d+"))) -> lights
lights %>%
  rename(i = rowid) %>%
  rename(j = colid) %>%
  select(-on) %>%
  mutate(neigh_i = i - 1, neigh_j = j - 1) %>%
  rbind(lights.idx %>% mutate(neigh_i = i - 1, neigh_j = j)) %>%
  rbind(lights.idx %>% mutate(neigh_i = i - 1, neigh_j = j + 1)) %>%
  rbind(lights.idx %>% mutate(neigh_i = i, neigh_j = j - 1)) %>%
  rbind(lights.idx %>% mutate(neigh_i = i, neigh_j = j + 1)) %>%
  rbind(lights.idx %>% mutate(neigh_i = i + 1, neigh_j = j - 1)) %>%
  rbind(lights.idx %>% mutate(neigh_i = i + 1, neigh_j = j)) %>%
  rbind(lights.idx %>% mutate(neigh_i = i + 1, neigh_j = j + 1)) -> lights.neigh

for (k in 1:100) {
  lights %>%
    inner_join(lights.neigh, by = c("rowid" = "i", "colid" = "j")) %>%
    inner_join(lights, by = c("neigh_i" = "rowid", "neigh_j" = "colid"), suffix = c("", "_neigh")) %>%
    group_by(rowid, colid) %>%
    summarise(on = ifelse(any(on), sum(on_neigh) %in% 2:3, sum(on_neigh) == 3), .groups = "drop") -> lights
  print(sum(lights$on))
}
