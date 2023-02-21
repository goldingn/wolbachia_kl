#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wolbachia_path
#' @return
#' @author njtierney
#' @export
read_release_site_info <- function(wolbachia_path) {

  release_site_info_raw <- read_excel(
    path = wolbachia_path,
    sheet = 1,
    .name_repair = janitor::make_clean_names
    )

  release_site_info_raw %>%
    mutate(
      across(
        c(
          start_release,
          stop_release,
          last_monitoring
          ),
        .fns = year_week_to_date
      )
    ) %>%
    separate_wider_delim(
      cols = site,
      delim = "_",
      names = c("ministry_code", "locality_name")
    )

}
