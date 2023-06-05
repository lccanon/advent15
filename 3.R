library(tidyverse)

get_mat <- function(dir) {
  mat <- matrix(0, nrow = length(dir), ncol = length(dir))

  init_i <- length(dir) / 2
  init_j <- length(dir) / 2
  mat[init_i,init_j] <- 1
  
  for (i in 1:length(dir)) {
    if (dir[i] == "^") {
      init_i <- init_i - 1
    } else if (dir[i] == "v") {
      init_i <- init_i + 1
    } else if (dir[i] == ">") {
      init_j <- init_j + 1
    } else if (dir[i] == "<") {
      init_j <- init_j - 1
    } else {
      print("Problem")
    }
    mat[init_i,init_j] <- 1
  }

  mat
}

dir <- (scan("input3", character(0)) %>%
          str_split(""))[[1]]

dir_santa <- dir[seq(1, length(dir), 2)]
dir_robo <- dir[seq(2, length(dir), 2)]

m1 <- get_mat(dir_santa)
m2 <- get_mat(dir_robo)

print(sum(pmax(m1, m2)))
