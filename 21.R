library(tidyverse)

player_hit_points <- 100

read_delim("input21", delim = ": ", col_names = c("property", "value")) %>%
  pivot_wider(names_from = "property", values_from = "value") -> boss

"Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0" %>%
  str_split("\\n") %>%
  unlist %>%
  tibble %>%
  separate(col = ".", into = c("Name", "Cost", "Damage", "Armor")) %>%
  mutate_at(2:4, as.integer) -> weapons

"Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5" %>%
  str_split("\\n") %>%
  unlist %>%
  tibble %>%
  separate(col = ".", into = c("Name", "Cost", "Damage", "Armor")) %>%
  mutate_at(2:4, as.integer) -> armors

"Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3" %>%
  str_split("\\n") %>%
  unlist %>%
  tibble %>%
  separate(col = ".", into = c("Name", "Effect", "Cost", "Damage", "Armor")) %>%
  select(-Effect) %>%
  mutate_at(2:4, as.integer) -> rings

min_cost <- sum(c(weapons$Cost, armors$Cost, rings$Cost))
max_cost <- 0
for (weap in 1:nrow(weapons))
  for (arm in 0:nrow(armors))
    for (ring1 in 0:nrow(rings))
      for (ring2 in ring1:nrow(rings)) {
        if (ring1 != 0 && ring1 == ring2)
          next
        equip <- weapons[weap,]
        if (arm != 0)
          equip <- rbind(equip, armors[arm,])
        if (ring1 != 0)
          equip <- rbind(equip, rings[ring1,])
        if (ring2 != 0)
          equip <- rbind(equip, rings[ring2,])
        cost <- sum(equip$Cost)
        damage <- sum(equip$Damage)
        armor <- sum(equip$Armor)
        player_to_boss <- max(1, damage - pull(boss, Armor))
        boss_to_player <- max(1, pull(boss, Damage) - armor)
        if (ceiling(player_hit_points / boss_to_player) >=
            ceiling(pull(boss, "Hit Points") / player_to_boss))
          min_cost <- min(min_cost, cost)
        if (ceiling(player_hit_points / boss_to_player) <
            ceiling(pull(boss, "Hit Points") / player_to_boss))
          max_cost <- max(max_cost, cost)
      }
print(min_cost)
print(max_cost)