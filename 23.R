library(tidyverse)

readLines("input23") %>%
  strsplit("\\n") %>%
  unlist %>%
  str_replace(",", "") %>%
  str_replace("([a-z])$", "\\1 0") %>%
  str_replace("jmp", "jmp X") %>%
  tibble %>%
  separate(col = ".", into = c("inst", "var", "val"), sep = " ") %>%
  mutate_at(3, as.integer) -> insts

vars <- list()
for (var in unique(insts$var))
  vars[[var]] <- 0
i <- 1
vars$a <- 1
while (i <= nrow(insts)) {
  if (insts[i,]$inst == "hlf") {
    vars[[insts[i,]$var]] <- vars[[insts[i,]$var]] / 2
    i <- i + 1
  } else if (insts[i,]$inst == "tpl") {
    vars[[insts[i,]$var]] <- vars[[insts[i,]$var]] * 3
    i <- i + 1
  } else if (insts[i,]$inst == "inc") {
    vars[[insts[i,]$var]] <- vars[[insts[i,]$var]] + 1
    i <- i + 1
  } else if (insts[i,]$inst == "jmp") {
    i <- i + insts[i,]$val
  } else if (insts[i,]$inst == "jie") {
    if (vars[[insts[i,]$var]] %% 2 == 0)
      i <- i + insts[i,]$val
    else
      i <- i + 1
  } else if (insts[i,]$inst == "jio") {
    if (vars[[insts[i,]$var]] == 1)
      i <- i + insts[i,]$val
    else
      i <- i + 1
  } else
    print("Problem")
}
print(vars$b)
