#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @return
#' @author njtierney
#' @export
panel_plot <- function(data) {
  data %>%
    mutate(
      incidence_pred = 100000 * post_pred_mean / population,
      incidence_lower = 100000 * post_pred_lower / population,
      incidence_upper = 100000 * post_pred_upper / population
    ) %>%
    ggplot(aes(
      x = date,
      y = incidence,
      colour = post_intervention
    )) +
    geom_line() +
    geom_ribbon(
      aes(
        ymin = incidence_lower,
        ymax = incidence_upper
      ),
      linetype = 0,
      alpha = 0.3
    ) +
    facet_wrap(
      ~ministry_code,
      ncol = 3,
      scales = "free_y"
    ) +
    scale_colour_discrete_qualitative() +
    theme(legend.position = "bottom") +
    labs(
      y = "dengue incidence (per 100,000)"
    )
}
