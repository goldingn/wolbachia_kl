#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wolbachia_path
#' @return
#' @author njtierney
#' @export
read_incidence_lookup <- function(wolbachia_path) {

  incidence_lookup <- read_excel(
    path = wolbachia_path,
    sheet = 4,
    range = "B1:CU3",
    .name_repair = janitor::make_clean_names
  ) %>%
    mutate(
      x = case_when(
        x == "Epid Week" ~ "ministry_code",
        .default = x
      )
    ) %>%
    rename(
      cols = x
    )

  incidence_lookup %>%
    pivot_longer(
      cols = -cols,
      names_to = "locality_name",
      values_to = "values"
    ) %>%
    pivot_wider(
      names_from = cols,
      values_from = values
    ) %>%
    relocate(
      ministry_code,
      locality_name,
      population
    )  %>%
    mutate(
      locality_name = case_when(
        str_detect(locality_name, "^x_") ~ NA,
        .default = locality_name
      ),
      population = parse_number(population)
    )

}
