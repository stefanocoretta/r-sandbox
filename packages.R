packages <- c(
  # CRAN ----

  "bayesplot",
  "blogdown",
  "bookdown",
  "brms",
  "devtools",
  "extraDistr",
  "ggdark",
  "ggdist",
  "HDInterval",
  "here",
  "leaflet",
  "logger",
  "remotes",
  "rgdal",
  "rgeos",
  "rnaturalearth",
  "rmarkdown",
  "rstan",
  "sf",
  "shiny",
  "sjPlot",
  "sp",
  "svglite",
  "tidybayes",
  "tidyverse",
  "Unicode",
  "visdat",
  "waffle",
  "wesanderson",
  "xaringan",
  "xaringanthemer",

  # GitHub ----

  "gadenbuie/xaringanExtra",
  "ropensci/rnaturalearthdata",
  "ropensci/rnaturalearthhires",
  "stan-dev/cmdstanr",

  # Mine ----

  "rticulate",
  "speakr",
  "tidymv"
)

renv::install(packages)
