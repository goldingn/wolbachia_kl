#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wolbachia_2023
#' @return
#' @author njtierney
#' @export
plot_ts_intervention <- function(wolbachia_2023) {

  wolbachia_2023 %>%
    filter(str_detect(ministry_code, "^W")) %>%
    ggplot(aes(
      x = date,
      y = incidence,
      colour = post_intervention
    )) +
    geom_line() +
    facet_wrap(~ministry_code,
               ncol = 3,
               scales = "free_y")

}
