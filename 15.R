library(tidyverse)

ingre <- readLines("input15") %>%
  str_replace("^(.*): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)$",
              "\\1,\\2,\\3,\\4,\\5,\\6") %>%
  tibble %>%
  separate(col = ".", into = c("name", "capacity", "durability", "flavor", "texture", "calories"), sep = ",") %>%
  mutate_at(-1, as.integer)

library(partitions)
comps <- t(compositions(100, nrow(ingre))) %>%
  as.matrix %>%
  `colnames<-` (ingre$name)

value <- list()
for (prop in names(ingre)[-1])
  value[[prop]] <- apply(comps %*% as.matrix(select(ingre, all_of(prop))), 1, sum)
value <- as_tibble(value)
value[value < 0] <- 0
value <- value %>%
  mutate(value = apply(select(value, -calories), 1, prod))
value %>%
  filter(calories == 500) %>%
  summarise(prod_max = max(value)) %>%
  pull
