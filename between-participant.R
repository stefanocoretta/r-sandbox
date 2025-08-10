library(brms)
library(dplyr)
library(ggplot2)

set.seed(123)

# Simulation parameters
n_languages <- 3
participants_per_language <- 10
obs_per_participant <- 5

# True fixed effects
beta_0 <- 10                  # Intercept (reference language mean)
beta_language <- c(0, 10, -2)  # Language effects: language1=0(ref), lang2=+3, lang3=-2

# Random intercept SD
sd_participant <- 2
sd_residual <- 1

# Create data frame
df <- expand.grid(
  participant = 1:(n_languages * participants_per_language),
  obs = 1:obs_per_participant
) %>%
  arrange(participant, obs) |>
  mutate(
    language = rep(rep(paste0("L", 1:n_languages), each = participants_per_language), each = obs_per_participant)
  )

# Assign participant-specific intercepts
participant_intercepts <- rnorm(n_languages * participants_per_language, 0, sd_participant)

# Generate response variable
df <- df %>%
  mutate(
    participant_intercept = participant_intercepts[participant],
    language_num = as.integer(factor(language)),
    mu = beta_0 + beta_language[language_num] + participant_intercept,
    y = rnorm(n(), mu, sd_residual)
  ) |>
  mutate(
    y = ifelse(participant == 30, y + 7, y)
  )



# Fit the brms model
fit <- brm(
  y ~ language + (1|participant),
  data = df,
  iter = 2000, chains = 2,
  seed = 123,
  control = list(adapt_delta = 0.95)
)

# Extract random intercepts (posterior means)
ranef_df <- ranef(fit)$participant[, , "Intercept"] %>%
  as.data.frame() %>%
  rename(
    mean_intercept = Estimate,
    lower = Q2.5,
    upper = Q97.5
  ) %>%
  mutate(participant = as.integer(rownames(.))) %>%
  left_join(
    df %>% distinct(participant, language),
    by = "participant"
  )

# Calculate language means (posterior)
fixef_fit <- fixef(fit) |>
  as_tibble() |>
  mutate(language = c("L1", "L2", "L3"))

ranef_df <- ranef_df |>
  left_join(y = fixef_fit |> select(Estimate, language)) |>
  mutate(
    mean_intercept_language = case_when(
      language == "L1" ~ mean_intercept + Estimate,
      TRUE ~ mean_intercept + Estimate + fixef(fit)[1,1]
    )
  )


# Plot participant intercepts and language means
ggplot(ranef_df, aes(x = participant, y = mean_intercept_language)) +
  geom_point(size = 3, aes(color = language)) +
  geom_point(data = df |> group_by(participant) |> summarise(mean = mean(y)), aes(y = mean)) +
  labs(
    title = "Participant random intercepts shrunk toward reference intercept",
    y = "Participant intercept estimate (random effect)",
    x = "Participant"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_brewer(palette = "Set1")

