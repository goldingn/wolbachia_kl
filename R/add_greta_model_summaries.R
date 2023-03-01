#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param greta_analysis
#' @param wolbachia_2023
#' @return
#' @author njtierney
#' @export
add_greta_model_summaries <- function(greta_analysis, wolbachia_2023) {

  means <- greta_analysis$means

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

  wolbachia_2023_model_results


}
