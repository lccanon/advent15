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
