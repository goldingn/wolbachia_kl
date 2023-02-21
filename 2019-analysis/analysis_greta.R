# fitting the model in greta

wol <- readRDS("data/clean/wol.rds")

# add a numeric ID for cluster
wol$cluster_id <- as.numeric(as.factor(wol$cluster))

n_times <- max(wol$week_id)
n_sites <- max(wol$location_id)

# build greta model
library(greta)
library(here)

# load code for efficient-ish AR(1) models, via a few methods
source(here("R/ar1ify.R"))

# build AR(1) timeseries effects
rho <- uniform(-1, 1)
sigma <- normal(0, 10, truncation = c(0, Inf))

innovations_raw <- normal(0, 1, dim = c(n_times - 1, n_sites))
innovations <- rbind(zeros(1, n_sites),
                     innovations_raw)
temporal_trends <- ar1(rho = rho,
                       n_times = n_times,
                       n_sites = n_sites,
                       sigma = sigma,
                       innovations = innovations)

location_effect <- normal(0, 10, dim = n_sites)

# pull out elements corresponding to observed data
idx <- cbind(wol$week_id, wol$location_id)
baseline_effect_vec <- temporal_trends[idx] +
  location_effect[wol$location_id]

# intervention effect
intervention_effect <- normal(0, 10)
intervention_vec <- intervention_effect * wol$post_intervention

# combine into linear predictor
eta <- baseline_effect_vec + intervention_vec

# gam(cases ~ location_id +
 # post_intervention +
 # s(week_id, location_id, bs = "ar1"),
 # family = poisson())

# add offset of population, scaled so that a prior mean for eta of 0 corresponds
# with the mean number of cases. Note that this does not affect inference, since
# the offset is combined with the mean temporal effect, however it will make MCMC easier.
multiplier <- mean(wol$cases / wol$population)
log_offset <- log(wol$population * multiplier)

# # this ratio should be around 1 for good sampling
# exp(mean(log_offset)) / mean(wol$cases)

# convert to the probs and sizes arguments of the negative binomial distribution
mean <- exp(eta + log_offset)

# likelihood
distribution(wol$cases) <- poisson(mean)

m <- model(intervention_effect, rho, sigma)
draws <- mcmc(m,
              warmup = 2000,
              n_samples = 2000,
              sampler = hmc(Lmin = 30, Lmax = 40))

draws <- extra_samples(draws, 2000)

# save draws
readr::write_rds(draws, here("data/clean/wol_draws.rds"))
# load draws
draws <- readr::read_rds(here("data/clean/wol_draws.rds"))

# check convergence
plot(draws)
coda::gelman.diag(draws)
coda::effectiveSize(draws)

# do posterior simulations (calculate size and prob, stick in rnbinom), summarise and plot as in INLA analysis
means <- as.matrix(calculate(mean, values = draws))

y_preds <- means * NA
y_preds[] <- rpois(prod(dim(means)), means[])

# summarise these
y_means <- colMeans(y_preds)
medians <- apply(means, 2, median)
y_CIs <- apply(y_preds, 2, quantile, c(0.025, 0.975))

# get objects required by DHARMa
library(DHARMa)
dharma <- createDHARMa(simulatedResponse = t(y_preds),
                       observedResponse = wol$cases,
                       fittedPredictedResponse = medians,
                       integerResponse = TRUE)

# annoyingly, these required objects aren't added by createDHARMa
dharma$refit <- FALSE
dharma$simulatedResponse <- t(y_preds)

testResiduals(dharma)

# timeseries plots of posterior predictive distribution
wol_out <- wol
wol_out$lower <- y_CIs[1, ]
wol_out$upper <- y_CIs[2, ]
wol_out$mean <- y_means

panel_plot <- function (data) {
  data %>%
    mutate(incidence = 100000 * cases / population,
           incidence_pred = 100000 * mean / population,
           incidence_lower = 100000 * lower / population,
           incidence_upper = 100000 * upper / population) %>%
    ggplot(aes(x = date, y = incidence, colour = post_intervention)) +
    geom_line() +
    geom_ribbon(aes(ymin = incidence_lower,
                    ymax = incidence_upper),
                linetype = 0,
                alpha = 0.3) +
    facet_wrap(~location, ncol = 1) +
    theme(legend.position = "bottom")
}

library(ggplot2)
library(tidyverse)

gg_wol_out_a <- wol_out %>%
  filter(cluster == "A") %>%
  panel_plot()

gg_wol_out_m <- wol_out %>%
  filter(cluster == "M") %>%
  panel_plot()

gg_wol_out_s <- wol_out %>%
  filter(cluster == "S") %>%
  panel_plot()

gg_wol_out_c <- wol_out %>%
  filter(cluster == "C") %>%
  panel_plot()

ggsave(here("plots/gg_wol_out_a.png"), plot = gg_wol_out_a, width = 8, height = 10)
ggsave(here("plots/gg_wol_out_m.png"), plot = gg_wol_out_m, width = 8, height = 10)
ggsave(here("plots/gg_wol_out_s.png"), plot = gg_wol_out_s, width = 8, height = 10)
ggsave(here("plots/gg_wol_out_c.png"), plot = gg_wol_out_c, width = 8, height = 10)

# summarise parameter of interest
perc_change <- 100 * (exp(intervention_effect) - 1)
perc_change_draws <- calculate(perc_change, values = draws)
summary(perc_change_draws)

perc_change_draws_vec <- as.matrix(perc_change_draws)[, 1]

# posterior probability of a reduction
mean(perc_change_draws_vec < 0)

# 95% CI
quantile(perc_change_draws_vec, c(0.025, 0.975))

# plot model fit (and data) for paper
means_CI <- apply(means, 2, quantile, c(0.025, 0.975))
means_mean <- colMeans(means)

wol_out <- wol
wol_out$lower <- means_CI[1, ]
wol_out$upper <- means_CI[2, ]
wol_out$mean <- means_mean

library(ggplot2)
library(tidyverse)

intervention_label <- data.frame(
  location = "A. Mentari Court",
  date = as.Date("2018-08-01"),
  incidence_pred = 160
)

unique(wol_out$location)

# plot only intervention sites
wol_out_intervention_site <- wol_out %>%
  # subset to and rename release sites
  mutate(
    release_site = location %in%
      c("MH", "SF", "SC", "AL", "AF", "SL")
  ) %>%
  filter(release_site) %>%
  mutate(location = case_when(
    location == "MH" ~ "A. Mentari Court",
    location == "SF" ~ "B. Section 7 Flats",
    location == "SC" ~ "C. Section 7 Commercial Centre",
    location == "AL" ~ "D. AU2 Landed",
    location == "AF" ~ "E. AU2 PKNS",
    location == "SL" ~ "F. Section 7 Landed"
  )) %>%
  # add incidence data (observed and expected)
  mutate(incidence = 100000 * cases / population,
         incidence_pred = 100000 * mean / population,
         incidence_lower = 100000 * lower / population,
         incidence_upper = 100000 * upper / population) %>%
  # data for shaded regions
  # mutate(ymax = Inf * post_intervention)
  # ggplot won't let you use Inf in geom_area anymore
  mutate(ymax = (max(incidence) * 1.2) * post_intervention,
         .by = location)

gg_wol_out_intervention_site <- ggplot(wol_out_intervention_site,
         aes(x = date, y = incidence_pred)) +
  geom_area(
    aes(y = ymax),
    alpha = 0.4,
    fill = "lightblue") +
  geom_line(size = 0.25) +
  geom_ribbon(aes(ymin = incidence_lower,
                  ymax = incidence_upper),
              linetype = 0,
              alpha = 0.2) +
  geom_point(
    aes(x = date, y = incidence),
    col = grey(0.2),
    size = 0.25
  ) +
  scale_y_continuous("dengue incidence (per 100,000)", minor_breaks = NULL) +
  xlab("") +
  facet_wrap(~location, ncol = 1, scales = "free") +
  geom_text(
    data = intervention_label,
    label = "post-release",
    size = 3.5
  ) +
  theme_minimal() +
  theme(strip.text = element_text(hjust = 0.5, size = 12))

gg_wol_out_intervention_site

ggsave(here("plots/final_plot.jpg"),
       plot = gg_wol_out_intervention_site,
       width = 8,
       height = 10)

# ggsave("Plot.png")

save(list = ls(all = TRUE), file = "cache.rda")
