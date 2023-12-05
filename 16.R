library(tidyverse)

"children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1" %>%
  str_split_1("\\n") %>%
  tibble %>%
  separate(col = ".", into = c("name", "quant"), sep = ": ") %>%
  mutate_at(2, as.integer) -> const

readLines("input16") %>%
  str_replace("^Sue (\\d*): (.*): (\\d+), (.*): (\\d+), (.*): (\\d+)$",
              "\\1,\\2,\\3,\\4,\\5,\\6,\\7") %>%
  tibble %>%
  separate(col = ".", into = c("number", "name1", "quant1", "name2", "quant2", "name3", "quant3"), sep = ",") %>%
  pivot_longer(starts_with(c("name")), names_to = "type_n", values_to = "name") %>%
  pivot_longer(starts_with(c("quant")), names_to = "type_q", values_to = "quant") %>%
  filter(str_extract(type_n, "\\d") == str_extract(type_q, "\\d")) %>%
  mutate(quant = as.integer(quant)) %>%
  select(-type_n, -type_q) -> aunts

# bad_aunts <- aunts %>%
#   left_join(const, by = "name") %>%
#   filter(quant.x != quant.y) %>%
#   pull(number)
bad_aunts <- aunts %>%
  left_join(const, by = "name") %>%
  filter(name %in% c("cats", "trees") & quant.x <= quant.y |
           name %in% c("pomeranians", "goldfish") & quant.x >= quant.y |
           !(name %in% c("cats", "trees", "pomeranians", "goldfish")) & quant.x != quant.y) %>%
  pull(number)
aunts %>%
  filter(!number %in% bad_aunts) %>%
  pull(number) %>%
  unique %>%
  print
