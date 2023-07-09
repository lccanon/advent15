library(tidyverse)

player_hit_points <- 50
player_mana <- 500

read_delim("input22", delim = ": ", col_names = c("property", "value")) %>%
  pivot_wider(names_from = "property", values_from = "value") -> boss

spells <- tribble(
  ~cost, ~damage, ~heal, ~duration_shield, ~duration_poison, ~duration_recharge,
  53, 4, 0, 0, 0, 0,
  73, 2, 2, 0, 0, 0,
  113, 0, 0, 6, 0, 0,
  173, 0, 0, 0, 6, 0,
  229, 0, 0, 0, 0, 5,
)

strats <- tibble(player_mana = player_mana,
                 player_hit_points = player_hit_points,
                 boss_hit_points = boss$`Hit Points`,
                 player_armor = 0,
                 shield_remaining = 0,
                 poison_remaining = 0,
                 recharge_remaining = 0,
                 total_mana = 0)

best_mana <- Inf
for (i in 1:(player_hit_points + boss$`Hit Points`)) {
  if (i %% 2 == 1) {
    # Player turn
    strats %>%
      crossing(spells) %>%
      # Effects cannot be stacked
      filter(duration_shield == 0 | shield_remaining == 0) %>%
      filter(duration_poison == 0 | poison_remaining == 0) %>%
      filter(duration_recharge == 0 | recharge_remaining == 0) %>%
      # Level is hard
      mutate(player_hit_points = player_hit_points - 1) %>%
      # Remove losing strats
      filter(player_hit_points > 0) %>%
      # Mana is handled
      mutate(player_mana = player_mana - cost) %>%
      mutate(total_mana = total_mana + cost) %>%
      filter(player_mana >= 0) %>%
      filter(total_mana < best_mana) %>%
      # Let's apply the selected action
      mutate(boss_hit_points = boss_hit_points - damage) %>%
      mutate(player_hit_points = player_hit_points + heal) %>%
      mutate(shield_remaining = pmax(shield_remaining, duration_shield)) %>%
      mutate(poison_remaining = pmax(poison_remaining, duration_poison)) %>%
      mutate(recharge_remaining = pmax(recharge_remaining, duration_recharge)) %>%
      select(names(strats)) -> strats
    if (nrow(strats) == 0)
      break
    # Optimize by pruning less efficient strats
    strats %>%
      group_by(player_hit_points, player_mana, boss_hit_points, player_armor, shield_remaining, poison_remaining, recharge_remaining) %>%
      summarise(total_mana = min(total_mana), .groups = "drop") %>%
      group_by(total_mana, player_mana, boss_hit_points, player_armor, shield_remaining, poison_remaining, recharge_remaining) %>%
      summarise(player_hit_points = max(player_hit_points), .groups = "drop") %>%
      group_by(player_hit_points, total_mana, player_mana, player_armor, shield_remaining, poison_remaining, recharge_remaining) %>%
      summarise(boss_hit_points = min(boss_hit_points), .groups = "drop") -> strats
    # print(nrow(strats))
  } else
    strats %>%
    # Boss turn is more easy and we remove successful strats (no longer useful)
    mutate(player_hit_points = player_hit_points - pmax(1, boss$Damage - player_armor)) %>%
    filter(boss_hit_points > 0) -> strats
  
  # Apply effects (shield has no effect at its last turn)
  strats %>%
    mutate(player_armor = ifelse(shield_remaining > 1, 7, 0)) %>%
    mutate(boss_hit_points = boss_hit_points - ifelse(poison_remaining > 0, 3, 0)) %>%
    mutate(player_mana = player_mana + ifelse(recharge_remaining > 0, 101, 0)) %>%
    mutate(shield_remaining = pmax(0, shield_remaining - 1)) %>%
    mutate(poison_remaining = pmax(0, poison_remaining - 1)) %>%
    mutate(recharge_remaining = pmax(0, recharge_remaining - 1)) -> strats
  
  # After applying actions and effects, we can check the best strat
  strats %>%
    filter(boss_hit_points <= 0) %>%
    pull(total_mana) %>%
    min(best_mana) -> best_mana
}
print(best_mana)

# Cleaner (arbitrary effects can be added) but slower (rowwise) alternative

spells <- tribble(
  ~name,    ~cost, ~damage, ~heal, ~shield, ~mana, ~duration,
  "missile",   53, 4, 0, 0, 0,   1,
  "drain",     73, 2, 2, 0, 0,   1,
  "shield",   113, 0, 0, 7, 0,   6,
  "poison",   173, 3, 0, 0, 0,   6,
  "recharge", 229, 0, 0, 0, 101, 5,
)

strats <- tibble(player_mana = player_mana,
                 player_hit_points = player_hit_points,
                 player_armor = 0,
                 boss_hit_points = boss$`Hit Points`,
                 remaining = list(list()),
                 total_mana = 0)

best_mana <- Inf
for (i in 1:(player_hit_points + boss$`Hit Points`)) {
  if (i %% 2 == 1) {
    # Player turn
    strats %>%
      crossing(spells %>% select(name, cost, duration)) %>%
      # Effects cannot be stacked
      rowwise %>% filter(!name %in% names(remaining)) %>% ungroup %>%
      # Level is hard
      mutate(player_hit_points = player_hit_points - 1) %>%
      # Cost of effect is handled
      mutate(player_mana = player_mana - cost) %>%
      mutate(total_mana = total_mana + cost) %>%
      # Duration of effect
      rowwise %>% mutate(remaining = { remaining[[name]] <- duration; list(remaining) }) %>%
      select(names(strats)) -> strats
    # Remove losing and unfeasible strats
    strats %>% filter(player_hit_points > 0) %>%
      filter(player_mana >= 0) %>%
      filter(total_mana < best_mana) -> strats
    if (nrow(strats) == 0)
      break
  } else
    strats %>%
      # Boss turn is easier and we remove successful strats (no longer useful)
      mutate(player_hit_points = player_hit_points - pmax(1, boss$Damage - player_armor)) %>%
      filter(boss_hit_points > 0) -> strats

  # Apply effects
  strats %>%
    # Compute effects
    rowwise %>%
    mutate(damage = sum(spells[spells$name %in% names(remaining),"damage"])) %>%
    mutate(heal = sum(spells[spells$name %in% names(remaining),"heal"])) %>%
    mutate(shield = sum(spells[spells$name %in% names(remaining),"shield"])) %>%
    mutate(mana = sum(spells[spells$name %in% names(remaining),"mana"])) %>%
    ungroup %>%
    # Apply effects
    mutate(boss_hit_points = boss_hit_points - damage) %>%
    mutate(player_hit_points = player_hit_points + heal) %>%
    mutate(player_armor = shield) %>%
    mutate(player_mana = player_mana + mana) %>%
    select(names(strats)) %>%
    # Decrease duration
    rowwise %>% mutate(remaining = list(map(remaining, ~ . - 1))) %>%
    # Prune effect with zero duration
    mutate(remaining = list(keep(remaining, ~ . > 0))) %>%
    ungroup -> strats

  # Optimize by pruning less efficient strats
  strats %>%
    group_by(player_hit_points, player_mana, boss_hit_points, player_armor, remaining) %>%
    summarise(total_mana = min(total_mana), .groups = "drop") %>%
    group_by(total_mana, player_mana, boss_hit_points, player_armor, remaining) %>%
    summarise(player_hit_points = max(player_hit_points), .groups = "drop") %>%
    group_by(player_hit_points, total_mana, player_mana, player_armor, remaining) %>%
    summarise(boss_hit_points = min(boss_hit_points), .groups = "drop") -> strats
  
  # Now that effects are applied, we can check the best strat
  strats %>%
    filter(boss_hit_points <= 0) %>%
    pull(total_mana) %>%
    min(best_mana) -> best_mana
}
print(best_mana)
