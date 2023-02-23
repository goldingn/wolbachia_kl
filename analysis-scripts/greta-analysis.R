## Load packages
source("./packages.R")

## Load all R files
lapply(list.files("./R", full.names = TRUE), source)

wolbachia_2023 <- targets::tar_read(wolbachia_2023)
n_warmup <- 1000
n_samples <- 1000
n_extra_draws <- 1000
Lmin <- 30
Lmax <- 40

n_times <- max(wolbachia_2023$week_id)
n_sites <- max(wolbachia_2023$location_id)

# build AR(1) timeseries effects
rho <- uniform(-1, 1)
sigma <- normal(0, 10, truncation = c(0, Inf))

innovations_raw <- normal(0, 1, dim = c(n_times - 1, n_sites))
innovations <- rbind(
  zeros(1, n_sites),
  innovations_raw
)
temporal_trends <- ar1(
  rho = rho,
  n_times = n_times,
  n_sites = n_sites,
  sigma = sigma,
  innovations = innovations
)

location_effect <- normal(0, 10, dim = n_sites)

# pull out elements corresponding to observed data
idx <- cbind(wolbachia_2023$week_id, wolbachia_2023$location_id)
baseline_effect_vec <- temporal_trends[idx] +
  location_effect[wolbachia_2023$location_id]

# intervention effect
intervention_effect <- normal(0, 10)
intervention_vec <- intervention_effect * wolbachia_2023$post_intervention

# combine into linear predictor
eta <- baseline_effect_vec + intervention_vec

# gam(cases ~ location_id +
# post_intervention +
# s(week_id, location_id, bs = "ar1"),
# family = poisson())

# add offset of population, scaled so that a prior mean for eta of 0
# corresponds with the mean number of cases. Note that this does not affect
# inference, since the offset is combined with the mean temporal effect,
# however it will make MCMC easier.

multiplier <- mean(wolbachia_2023$cases / wolbachia_2023$population)
log_offset <- log(wolbachia_2023$population * multiplier)

# # this ratio should be around 1 for good sampling
# exp(mean(log_offset)) / mean(wolbachia_2023$cases)

# convert to the probs and sizes arguments of the negative binomial distribution
mean <- exp(eta + log_offset)

# likelihood
distribution(wolbachia_2023$cases) <- poisson(mean)

m <- model(intervention_effect, rho, sigma)

draws <- mcmc(
  m,
  warmup = n_warmup,
  n_samples = n_samples,
  sampler = hmc(Lmin = Lmin, Lmax = Lmax)
)

draws <- extra_samples(draws, n_samples = n_extra_draws)

# summarise parameter of interest
perc_change <- 100 * (exp(intervention_effect) - 1)
perc_change_draws <- calculate(perc_change, values = draws)
parameter_summary <- summary(perc_change_draws)

perc_change_draws_vec <- as.matrix(perc_change_draws)[, 1]

# do posterior simulations (calculate size and prob, stick in rnbinom),
# summarise and plot as in INLA analysis
means <- as.matrix(calculate(mean, values = draws))

# dims are 4 * n_samples x 43848 (nrows in data)
y_preds <- means * NA
y_preds[] <- rpois(prod(dim(means)), means[])

# timeseries plots of posterior predictive distribution
y_means <- colMeans(y_preds)
y_CIs <- apply(y_preds, 2, quantile, c(0.025, 0.975))

# plot model fit (and data) for paper
means_CI <- apply(means, 2, quantile, c(0.025, 0.975))
means_mean <- colMeans(means)

wolbachia_2023_model_results <- wolbachia_2023 %>%
  mutate(
    post_pred_lower = y_CIs[1, ],
    post_pred_upper = y_CIs[2, ],
    post_pred_mean = y_means,
    model_lower = means_CI[1, ],
    model_upper = means_CI[2, ],
    model_mean = means_mean
  )

write_rds(
  draws,
  here("analysis-results/draws.rds")
)
write_rds(
  means,
  here("analysis-results/means.rds")
)
write_rds(
  wolbachia_2023_model_results,
  here("analysis-results/wolbachia_2023_model_results.rds")
)
write_rds(
  parameter_summary,
  here("analysis-results/parameter_summary.rds")
)
write_rds(
  perc_change_draws_vec,
  here("analysis-results/perc_change_draws_vec.rds")
)
