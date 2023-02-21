# create a greta operation for an AR(1) process, given innovations and parameters

# given a matrix greta array of random normal deviates for 'innovations', and
# scalar greta array for `rho` (between -1 and 1) and `sigma` (positive),
# return a matrix of the same dimension as `innovations`, with AR(1) process
# correlations. Do this by iterating forward in time.
# Simulate AR1 efficiently in TensorFlow by expanding the iterative equation:
#   X_t = \rho X_{t-1} + epsilon_t
# to
#   X_t = \Sum_{i=0}^{t}(\epsilon_{t - i} \rho ^ i)
# which can be simulated with elementwise matrix operations, and a matrix
# multiplication. Sigma can be a vector with a different element per site
ar1 <- function (rho, n_times, n_sites = 1, sigma = 1,
                 innovations = normal(0, 1, dim = c(n_times, n_sites))) {

  # matrix of time modifiers
  t_seq <- seq_len(n_times)
  t_mat <- outer(t_seq, t_seq, FUN = "-")
  t_mat <- pmax(t_mat, 0)

  # which elements to include (don't want upper triangular ones)
  mask <- lower.tri(t_mat, diag = TRUE)

  # matrix of rho ^ n contributions
  rho_mat <- (rho ^ t_mat) * mask

  # multiply by scaled innovations to get timeseries
  if (length(sigma) == n_sites) {
    innovations_scaled <- sweep(innovations, 2, sigma, "*")
  } else if(length(sigma) == 1) {
    innovations_scaled <- innovations * sigma
  } else {
    stop ("sigma must have either length 1 or length n_sites")
  }
  rho_mat %*% innovations_scaled

}

