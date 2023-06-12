library(tidyverse)

gift <- read_delim("input2", delim = "x", col_names = c("l", "w", "h")) %>%
  mutate(paper = 2 * (l * w + l * h + w * h) + pmin(l * w, l * h, w * h),
         ribbon = l * w * h + 2 * pmin(l + w, l + h, w + h)) %>%
  summarise(paper = sum(paper), ribbon = sum(ribbon))
print(gift)
