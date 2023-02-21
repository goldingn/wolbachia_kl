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
  )

  release_control_sites

}
