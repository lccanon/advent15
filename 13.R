library(tidyverse)

hp <- read_delim("input13", delim = " ",
           col_types = "c-ci------c",
           col_names = c("name1", NULL, "gain", "happy", rep(NULL, 6), "name2")) %>%
  mutate(happy = ifelse(gain == "gain", 1, -1) * happy) %>%
  mutate(name2 = str_replace(name2, "\\.", "")) %>%
  select(-gain) %>%
  mutate(name1 = factor(name1)) %>%
  mutate(name2 = factor(name2, levels(name1)))
# Symmetrical weights
hp <- hp %>%
  inner_join(hp, by = c("name2" = "name1", "name1" = "name2"),
             suffix = c("", ".rev")) %>%
  mutate(happy = happy + happy.rev) %>%
  select(-happy.rev)

# New person
person <- unique(hp$name1)
hp <- hp %>%
  rbind(tibble(name1 = "A", happy = 0, name2 = person)) %>%
  rbind(tibble(name1 = person, happy = 0, name2 = "A"))

person <- unique(hp$name1)
seating <- tibble(first_person = person, last_person = person, happy = 0,
                  all_persons = 2^as.numeric(person))
for (i in 2:length(person)) {
  seating <- seating %>%
    inner_join(hp, by = c("last_person" = "name1"), suffix = c("", ".next")) %>%
    filter(bitwAnd(all_persons, 2^as.numeric(name2)) == 0) %>%
    mutate(happy = happy + happy.next) %>%
    mutate(last_person = name2) %>%
    mutate(all_persons = all_persons + 2^as.numeric(name2)) %>%
    select(-happy.next, -name2) %>%
    # optimization to prune inefficient solutions
    group_by(first_person, last_person, all_persons) %>%
    summarise(happy = max(happy), .groups = "drop")
  # print(nrow(seating))
}

# Make the seating arrangement circular
seating <- seating %>%
  inner_join(hp, by = c("first_person" = "name1", "last_person" = "name2"),
             suffix = c("", ".next")) %>%
  mutate(happy = happy + happy.next) %>%
  select(-happy.next)
seating %>%
  summarise(happy = max(happy)) %>%
  pull %>%
  print
