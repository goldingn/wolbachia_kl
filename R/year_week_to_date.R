#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param year_week
#' @return
#' @author njtierney
#' @export
year_week_to_date <- function(year_week) {

  year <- as.numeric(str_split_i(year_week, "/", 1)) + 2000
  week <- as.numeric(str_split_i(year_week, "/", 2))
  date <- as.Date("0000-01-01") + years(year) + weeks(week - 1)
  date

}
