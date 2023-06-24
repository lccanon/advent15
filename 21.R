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
  mutate_at(-1, as.integer) -> weapons

"Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5" %>%
  str_split("\\n") %>%
  unlist %>%
  tibble %>%
  separate(col = ".", into = c("Name", "Cost", "Damage", "Armor")) %>%
  mutate_at(-1, as.integer) %>%
  rbind(0) -> armors

"Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3" %>%
  str_split("\\n") %>%
  unlist %>%
  str_replace(" \\+", "\\+") %>%
  tibble %>%
  separate(col = ".", into = c("Name", "Cost", "Damage", "Armor"), sep = "\\s+") %>%
  mutate_at(-1, as.integer) %>%
  rbind(0) %>%
  rbind(0) -> rings

min_cost <- sum(c(weapons$Cost, armors$Cost, rings$Cost))
max_cost <- 0
for (weap in 1:nrow(weapons))
  for (arm in 1:nrow(armors))
    for (ring1 in 1:(nrow(rings) - 1))
      for (ring2 in (ring1 + 1):nrow(rings)) {
        equip <- weapons[weap,] %>%
          rbind(armors[arm,]) %>%
          rbind(rings[ring1,]) %>%
          rbind(rings[ring2,])
        cost <- sum(equip$Cost)
        damage <- sum(equip$Damage)
        armor <- sum(equip$Armor)
        player_to_boss <- max(1, damage - boss$Armor)
        boss_to_player <- max(1, boss$Damage - armor)
        if (ceiling(player_hit_points / boss_to_player) >=
            ceiling(boss$`Hit Points` / player_to_boss))
          min_cost <- min(min_cost, cost)
        if (ceiling(player_hit_points / boss_to_player) <
            ceiling(boss$`Hit Points` / player_to_boss))
          max_cost <- max(max_cost, cost)
      }
print(min_cost)
print(max_cost)
