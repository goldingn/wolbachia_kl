## Load packages
source("./packages.R")

## Load all R files
lapply(list.files("./R", full.names = TRUE), source)

wolbachia_2023 <- targets::tar_read(wolbachia_2023) %>%
  # drop
  drop_na(fw)

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
# scaled by fw
intervention_effect_fw <- normal(0, 10)
intervention_fw_vec <- intervention_effect_fw * wolbachia_2023$fw

# combine into linear predictor
eta <- baseline_effect_vec + intervention_fw_vec

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

m <- model(intervention_effect_fw, rho, sigma)

library(tictoc)
tic()
draws_fw <- mcmc(
  m,
  warmup = n_warmup,
  n_samples = n_samples,
  sampler = hmc(Lmin = Lmin, Lmax = Lmax)
)

draws_fw <- extra_samples(draws_fw, n_samples = n_extra_draws)
toc()

# summarise parameter of interest
perc_change_fw <- 100 * (exp(intervention_effect_fw) - 1)
perc_change_draws_fw <- calculate(perc_change_fw, values = draws_fw)
parameter_summary_fw <- summary(perc_change_draws_fw)

perc_change_draws_fw_vec <- as.matrix(perc_change_draws_fw)[, 1]

# do posterior simulations (calculate size and prob, stick in rnbinom),
# summarise and plot as in INLA analysis
means_fw <- as.matrix(calculate(mean, values = draws_fw))

write_rds(
  draws_fw,
  here("analysis-results/draws_fw.rds")
)
write_rds(
  means_fw,
  here("analysis-results/means_fw.rds")
)
write_rds(
  parameter_summary_fw,
  here("analysis-results/parameter_summary_fw.rds")
)
write_rds(
  perc_change_draws_fw_vec,
  here("analysis-results/perc_change_draws_fw_vec.rds")
)


###

write_rds(
  wolbachia_2023_model_results_fw,
  here("analysis-results/wolbachia_2023_model_results_fw.rds")
)
