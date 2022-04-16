between <- list(pet = c(cat = "Cat Owners",
                        dog = "Dog Owners"))
within <- list(time = c("morning", "noon", "evening", "night"))
mu <- list(
  cat    = c(morning = 10, noon = 12, evening = 14, night = 16),
  dog    = c(morning = 10, noon = 15, evening = 20, night = 25)
)
# add factor labels for plotting
vardesc <- c(pet = "Type of Pet",
             time = "Time of Day")

df <- sim_design(within, between,
                 n = 100, mu = mu, sd = 5, r = .5,
                 empirical = TRUE, vardesc = vardesc, plot = TRUE)

df <- sim_design(within = c(2, 3, 2), between = c(2, 2),
                 n = 10, mu = 1:48, sd = rnorm(48, 30, 8), r = 0.5)
