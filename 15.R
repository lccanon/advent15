library(tidyverse)

ingre <- readLines("input15") %>%
  str_replace("^(.*): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)$",
              "\\1,\\2,\\3,\\4,\\5,\\6") %>%
  tibble %>%
  separate(col = ".", into = c("name", "capacity", "durability", "flavor", "texture", "calories"), sep = ",") %>%
  mutate_at(-1, as.integer)

# prod_max <- 0
# for (i in 0:100) {
#   print(c(i, prod_max))
#   for (j in 0:(100 - i))
#     for (k in 0:(100 - i - j)) {
#       l <- 100  - i - j - k
#       cal <- sum(t(c(i, j, k, l)) * ingre[,"calories"])
#       if (cal != 500)
#         next
#       curr <- prod(apply(t(c(i, j, k, l)) * select(ingre, -name, -calories), 2,
#                          function(x) max(0, sum(x))))
#       prod_max <- max(curr, prod_max)
#     }
# }
# print(prod_max)

library(partitions)
comps <- t(compositions(100, nrow(ingre)))
colnames(comps) <- ingre$name
comps <- comps %>%
  as.matrix %>%
  as.tibble

for (prop in names(ingre)[-1]) {
  comps <- comps %>%
    mutate(prop = apply(as.matrix(comps[,1:nrow(ingre)]) %*% as.matrix(select(ingre, all_of(prop))), 1, sum)) %>%
    rename_with(~ prop, prop)
}
comps[comps < 0] <- 0
comps <- comps %>%
  mutate(value = apply(select(comps, names(ingre)[-1] %>% head(-1)), 1, prod))
comps %>%
  filter(calories == 500) %>%
  summarise(prod_max = max(value)) %>%
  pull
