#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wolbachia_path
#' @return
#' @author njtierney
#' @export
read_cases_lookup <- function(wolbachia_path) {

  cases_lookup <- read_excel(
    path = wolbachia_path,
    sheet = 3,
    range = "B1:CU3",
    .name_repair = janitor::make_clean_names
  ) %>%
    mutate(
      x = case_when(
        x == "Population" ~ "population",
        x == "Epid Week" ~ "location",
      )
    ) %>%
    rename(
      cols = x
    )

  cases_lookup %>%
    pivot_longer(
      cols = -cols,
      names_to = "full_name",
      values_to = "values"
    ) %>%
    pivot_wider(
      names_from = cols,
      values_from = values
    ) %>%
    relocate(
      location,
      full_name,
      population
    ) %>%
    mutate(
      full_name = case_when(
        str_detect(full_name, "^x_") ~ NA,
        .default = full_name
      ),
      population = parse_number(population)
    ) %>%
    rename(
      ministry_code = location,
      locality_name = full_name
    )

}
