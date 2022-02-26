library(tidyverse)
library(brms)

cats <- tibble(
  y = rep(c(rep("K", 20), rep("T", 40), rep("B", 40)), 2),
  x = rep(c("A", "B"), each = 100)
)

cats_bm <- brm(y ~ x, data = cats, family = categorical)
