#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param means_fw
#' @param wolbachia_2023
#' @return
#' @author njtierney
#' @export
add_greta_model_summaries_fw <- function(means_fw, wolbachia_2023) {

  wolbachia_2023 <- wolbachia_2023 %>% drop_na(fw)

  # dims are 4 * n_samples x 43848 (nrows in data)
  y_preds_fw <- means_fw * NA
  y_preds_fw[] <- rpois(prod(dim(means_fw)), means_fw[])

  # timeseries plots of posterior predictive distribution
  y_means_fw <- colMeans(y_preds_fw)
  y_CIs_fw <- apply(y_preds_fw, 2, quantile, c(0.025, 0.975))

  # plot model fit (and data) for paper
  means_CI_fw <- apply(means_fw, 2, quantile, c(0.025, 0.975))
  means_mean_fw <- colMeans(means_fw)

  wolbachia_2023_model_results_fw <- wolbachia_2023 %>%
    mutate(
      post_pred_lower = y_CIs_fw[1, ],
      post_pred_upper = y_CIs_fw[2, ],
      post_pred_mean = y_means_fw,
      model_lower = means_CI_fw[1, ],
      model_upper = means_CI_fw[2, ],
      model_mean = means_mean_fw
    )

}
