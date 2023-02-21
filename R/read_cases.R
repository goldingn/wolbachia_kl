#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wolbachia_path
#' @return
#' @author njtierney
#' @export
read_cases <- function(wolbachia_path) {

  cases_raw <- read_excel(
    path = wolbachia_path,
    sheet = 3,
    skip = 2,
    .name_repair = janitor::make_clean_names
  )

  cases_raw %>%
    mutate(
      date = as.Date("0000-01-01") +
        years(year) +
        weeks(epid_week - 1),
      .before = everything()
    ) %>%
    pivot_longer(
      cols = -c(date, year, epid_week),
      names_to = "ministry_code",
      values_to = "cases"
    ) %>%
    arrange(
      ministry_code,
      date
    ) %>%
    mutate(
      ministry_code = toupper(ministry_code)
    ) %>%
    relocate(
      ministry_code
      )

}
