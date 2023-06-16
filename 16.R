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
  strsplit("\\n") %>%
  unlist %>%
  tibble %>%
  separate(col = ".", into = c("name", "quant"), sep = ": ") %>%
  mutate_at(2, as.integer) -> const

readLines("input16") %>%
  str_replace("^Sue (\\d*): (.*): (\\d+), (.*): (\\d+), (.*): (\\d+)$",
              "\\1,\\2,\\3,\\4,\\5,\\6,\\7") %>%
  tibble %>%
  separate(col = ".", into = c("number", "name1", "quant1", "name2", "quant2", "name3", "quant3"), sep = ",") %>%
  mutate_at(c(1, 3, 5, 7), as.integer) -> aunts

for (i in 1:nrow(const)) {
  print(nrow(aunts))
  for (j in c(2, 4, 6))
    if (pull(const[i,1]) %in% c("cats", "trees"))
      aunts <- aunts %>%
        filter(aunts[,j] != pull(const[i,], name) | aunts[,j + 1] > pull(const[i,], quant))
    else if (pull(const[i,1]) %in% c("pomeranians", "goldfish"))
      aunts <- aunts %>%
        filter(aunts[,j] != pull(const[i,], name) | aunts[,j + 1] < pull(const[i,], quant))
    else
      aunts <- aunts %>%
        filter(aunts[,j] != pull(const[i,], name) | aunts[,j + 1] == pull(const[i,], quant))
}
aunts %>%
  pull(number) %>%
  print

# Second solution

to_list <- function(str) {
  data <- str %>% str_split(": ") %>% unlist
  list(as.numeric(data[2])) %>%
    set_names(data[1])
}

aunts <- readLines("input16") %>%
  str_extract_all("([a-z]*): (\\d+)") %>%
  map(~ map(., to_list) %>% flatten)

index <- rep(TRUE, length(aunts))
for (i in 1:nrow(const)) {
  print(sum(index))
  if (pull(const[i,1]) %in% c("cats", "trees"))
    idx <- map(aunts, ~ .[[pull(const[i,1])]] > pull(const[i,], quant))
  else if (pull(const[i,1]) %in% c("pomeranians", "goldfish"))
    idx <- map(aunts, ~ .[[pull(const[i,1])]] < pull(const[i,], quant))
  else
    idx <- map(aunts, ~ .[[pull(const[i,1])]] == pull(const[i,], quant))
  index <- index & idx %>% map_lgl(~ replace(., length(.) == 0, values = TRUE))
}
print(which(index))

# Third solution

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

for (i in 1:nrow(const)) {
  print(nrow(aunts))
  removed_aunts <- aunts %>%
    filter(name == pull(const[i,], name))
  if (pull(const[i,1]) %in% c("cats", "trees"))
    removed_aunts <- filter(removed_aunts, quant <= pull(const[i,], quant))
  else if (pull(const[i,1]) %in% c("pomeranians", "goldfish"))
    removed_aunts <- filter(removed_aunts, quant >= pull(const[i,], quant))
  else
    removed_aunts <- filter(removed_aunts, quant != pull(const[i,], quant))
  aunts <- aunts %>%
    filter(!number %in% pull(removed_aunts, number))
}
aunts %>%
  pull(number) %>%
  unique %>%
  print
