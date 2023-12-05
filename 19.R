library(tidyverse)

data <- readLines("input19")

head(data, -2) %>%
  unlist %>%
  tibble %>%
  separate(col = ".", into = c("from", "to"), sep = " => ") %>%
  mutate(len = str_length(to)) -> eq

str <- tail(data, 1) %>%
  str_split_1("")

res <- list()
for (i in 1:length(str)) {
  # print(c(i, length(res)))
  cur1 <- str[i]
  for (replacement in (eq %>% filter(from == cur1) %>% pull(to)))
    res[[length(res) + 1]] <- c(head(str, i - 1), replacement, tail(str, -i)) %>%
      paste(collapse = "")
  if (i == 1)
    next
  cur2 <- str_c(str[i - 1], str[i])
  for (replacement in (eq %>% filter(from == cur2) %>% pull(to)))
    res[[length(res) + 1]] <- c(head(str, i - 2), replacement, tail(str, -i)) %>%
      paste(collapse = "")
}
print(length(unique(res)))

str <- tail(data, 1)
i <- str_length(str)
op <- 0
while (str_length(str) != 1) {
  for (j in 1:nrow(eq)) {
    pat <- eq[j,]$to
    if (str_sub(str, i, i + str_length(pat) - 1) != pat)
      next
    str <- str_replace(str, pat, eq[j,]$from)
    op <- op + 1
    i <- str_length(str)
    break
  }
  i <- i - 1
  # print(c(i, str_length(str)))
}
print(op)

# Actual solutions

str <- tail(data, 1)
str_count(str, "[A-Z][a-z]*") - str_count(str, "Rn|Ar") - 2 * str_count(str, "Y") - 1
