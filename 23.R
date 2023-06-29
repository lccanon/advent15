library(tidyverse)

read_lines("input23") %>%
  str_replace(",", "") %>%
  str_replace("([a-z])$", "\\1 NA") %>%
  str_replace("jmp", "jmp NA") %>%
  tibble %>%
  separate(col = ".", into = c("inst", "reg", "offset"), sep = " ") %>%
  mutate_at(3, as.integer) -> insts

regs <- list()
for (reg in unique(insts$reg))
  regs[[reg]] <- 0
i <- 1
regs$a <- 1
while (i <= nrow(insts)) {
  val <- regs[[insts[i,]$reg]]
  inst <- insts[i,]$inst
  regs[[insts[i,]$reg]] <- case_match(inst,
    "hlf" ~ val / 2,
    "tpl" ~ val * 3,
    "inc" ~ val + 1,
    .default = val
  )
  i <- case_when(
    inst == "jmp" ||
      inst == "jie" && val %% 2 == 0 ||
      inst == "jio" && val == 1 ~ i + insts[i,]$offset,
    TRUE ~ i + 1,
  )
}
print(regs$b)
