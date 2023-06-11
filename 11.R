library(tidyverse)

pass <- "hepxcrrq"
pp <- utf8ToInt(pass)
while (str_detect(pass, "i|o|l") ||
       str_count(pass, "(.)\\1") < 2 ||
       all(rle(diff(pp))$values != 1 | rle(diff(pp))$lengths < 2)) {
  for (i in length(pp):2) {
    if (pp[i] == utf8ToInt("z"))
      pp[i] <- utf8ToInt("a")
    else {
      pp[i] <- pp[i] + 1
      if (pp[i] %in% utf8ToInt("iol"))
        pp[i] <- pp[i] + 1
      break
    }
  }
  pass <- intToUtf8(pp)
}
print(pass)
