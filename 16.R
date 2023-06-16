library(tidyverse)

readLines("input16") %>%
  str_replace("^Sue (\\d*): (.*): (\\d+), (.*): (\\d+), (.*): (\\d+)$",
              "\\1,\\2,\\3,\\4,\\5,\\6,\\7") %>%
  tibble %>%
  separate(col = ".", into = c("number", "name1", "quant1", "name2", "quant2", "name3", "quant3"), sep = ",") %>%
  mutate_at(c(1, 3, 5, 7), as.integer) -> aunts

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

for (i in 1:nrow(const)) {
  print(nrow(aunts))
  if (pull(const[i,1]) %in% c("cats", "trees")) {
    aunts <- aunts %>% 
      filter(name1 == pull(const[i,], name) & quant1 > pull(const[i,], quant) |
               name2 == pull(const[i,], name) & quant2 > pull(const[i,], quant) |
               name3 == pull(const[i,], name) & quant3 > pull(const[i,], quant) |
               name1 != pull(const[i,], name) & name2 != pull(const[i,], name) & name3 != pull(const[i,], name))
  } else if (pull(const[i,1]) %in% c("pomeranians", "goldfish")) {
    aunts <- aunts %>% 
      filter(name1 == pull(const[i,1]) & quant1 < pull(const[i,2]) |
               name2 == pull(const[i,1]) & quant2 < pull(const[i,2]) |
               name3 == pull(const[i,1]) & quant3 < pull(const[i,2]) |
               name1 != pull(const[i,1]) & name2 != pull(const[i,1]) & name3 != pull(const[i,1]))
  } else {
    aunts <- aunts %>% 
      filter(name1 == pull(const[i,1]) & quant1 == pull(const[i,2]) |
               name2 == pull(const[i,1]) & quant2 == pull(const[i,2]) |
               name3 == pull(const[i,1]) & quant3 == pull(const[i,2]) |
               name1 != pull(const[i,1]) & name2 != pull(const[i,1]) & name3 != pull(const[i,1]))
  }
}

aunts %>%
  pull(number) %>%
  print
