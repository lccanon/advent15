library(tidyverse)

gift <- read.table("input2", sep = "x", col.names = c("l", "w", "h")) %>%
  as_tibble() %>%
  mutate(paper = 2 * (l * w + l * h + w * h) + pmin(l * w, l * h, w * h),
         ribbon = l * w * h + 2 * pmin(l + w, l + h, w + h)) %>%
  summarise(paper = sum(paper), ribbon = sum(ribbon))
