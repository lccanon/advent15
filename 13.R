library(tidyverse)

hp <- read_delim("input13", delim = " ",
           col_types = "c-ci------c",
           col_names = c("name1", "A", "gain", "happy", "B", "C", "D", "E", "F",
                         "G", "name2")) %>%
  mutate(happy = ifelse(gain == "gain", 1, -1) * happy) %>%
  mutate(name2 = str_extract(name2, "[^\\.]*")) %>%
  select(-gain) %>%
  mutate(name1 = factor(name1)) %>%
  mutate(name2 = factor(name2, levels(name1)))

person <- unique(hp$name1)
hp <- hp %>%
  rbind(tibble(name1 = "A", happy = 0, name2 = person)) %>%
  rbind(tibble(name1 = person, happy = 0, name2 = "A"))
person <- unique(hp$name1)
seating <- tibble(first_person = person, last_person = person, happy = 0,
                  all_persons = 2^as.numeric(person))

for (i in 2:length(person)) {
  seating <- seating %>%
    inner_join(hp, by = c("last_person" = "name1"), suffix = c("", "1")) %>%
    filter(bitwAnd(all_persons, 2^as.numeric(name2)) == 0) %>%
    inner_join(hp, by = c("last_person" = "name2"), suffix = c("", "2")) %>%
    filter(name1 == name2) %>%
    mutate(happy = happy + happy1 + happy2) %>%
    mutate(last_person = name1) %>%
    mutate(all_persons = all_persons + 2^as.numeric(name1)) %>%
    select(-happy1, -happy2, -name1, -name2) %>%
    # optimization to prune inefficient solutions
    group_by(first_person, last_person, all_persons) %>%
    summarise(happy = max(happy), .groups = "drop")
  # print(nrow(seating))
}

# Make the seating arrangement circular
seating <- seating %>%
  inner_join(hp, by = c("first_person" = "name1", "last_person" = "name2"), suffix = c("", "1")) %>%
  inner_join(hp, by = c("first_person" = "name2", "last_person" = "name1"), suffix = c("", "2")) %>%
  mutate(happy = happy + happy1 + happy2) %>%
  select(-happy1, -happy2)
seating %>%
  summarise(happy = max(happy)) %>%
  print
