library(tidyverse)

json <- readLines("input12")
print(json %>%
  str_extract_all("-?[0-9]+") %>%
  unlist %>%
  as.numeric %>%
  sum)

library(rjson)

parsed <- json %>%
  fromJSON(simplify = FALSE)

get_count <- function(tt) {
  if (!is.null(names(tt)) &&
      any(map_lgl(tt, ~ is_character(.) && . == "red")))
        return(0)
  count <- 0
  for (s in tt)
    if (is.numeric(s))
      count <- count + s
    else if (is.list(s))
      count <- count + get_count(s)
    else if (!is.character(s))
      print("Problem")
  count
}
print(get_count(parsed))
