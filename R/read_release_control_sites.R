#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wolbachia_path
#' @return
#' @author njtierney
#' @export
read_release_control_sites <- function(wolbachia_path) {

  release_control_sites <- read_excel(
    path = wolbachia_path,
    sheet = 2,
    .name_repair = janitor::make_clean_names
  ) %>%
    mutate(
      cluster = str_sub(final_kod, 1, 3),
      .after = final_kod
    ) %>%
    distinct(ministry_code, .keep_all = TRUE)

}
