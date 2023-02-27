#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wolbachia_2023
#' @return
#' @author njtierney
#' @export
plot_ts_cluster_figure <- function(wolbachia_2023, ref_cluster) {

  wolbachia_2023 %>%
    filter(
      cluster == ref_cluster,
    ) %>%
  ggplot(
    aes(
      x = date,
      y = incidence,
      colour = post_intervention
    )
  ) +
    geom_line() +
    facet_wrap(
      ~fct_relevel(ministry_code, ref_cluster),
      ncol = 1,
      scales = "free_y"
    ) +
    scale_colour_discrete_qualitative() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(
      y = "dengue incidence (per 100,000)"
    )
}
