library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

calc_likelihood <- function(mu, sigma, observations, wall) {
  prod(
    dnorm(observations, mu, sigma) /
      pnorm(wall, mu, sigma, lower.tail = FALSE)
  )
}

wall <- 180
observations <- c(181, 188, 190)

stat_df <- tibble(
  mean = mean(observations),
  std = sd(observations),
  likelihood = 0
)


df_mu <- tibble(
  mu = seq(170, 190, by = 0.1),
  prior_mu = 1 # dnorm(mu, 178, 10)
)

df_sigma <- tibble(
  sigma = seq(1, 20, by = 0.1),
  prior_sigma = 1 # dnorm(sigma, 10, 5)
)

df <- crossing(df_mu, df_sigma) %>%
  rowwise() %>% # if don't have then can't calculate posterior
  mutate(likelihood = calc_likelihood(mu, sigma, observations, wall)) %>% 
  ungroup() %>% 
  mutate(
    posterior_unstd = likelihood * prior_mu * prior_sigma,
    posterior = posterior_unstd / sum(posterior_unstd)
  )

top_value <- df %>%
  arrange(desc(posterior)) %>%
  head(n=1)

ggplot2::ggplot(df, aes(x = mu, y = sigma, z = likelihood, color = likelihood)) +
  geom_contour_filled() +
  geom_point(data = stat_df, aes(x = mean, y = std)) +
  theme_bw() +
  theme(legend.position = "none")
