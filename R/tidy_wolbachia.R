#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param release_site_info
#' @param release_control_sites
#' @param cases_lookup
#' @param cases
#' @param incidence_lookup
#' @param incidence
#' @return
#' @author njtierney
#' @export
tidy_wolbachia <- function(
  cases,
  incidence,
  cases_lookup,
  release_site_info,
  release_control_sites
    ) {

  cases_joins <- cases %>%
    left_join(
      incidence,
      by = c("ministry_code", "date", "year", "epid_week")
    ) %>%
    left_join(
      cases_lookup,
      by = "ministry_code"
    ) %>%
    relocate(
      population,
      .after = incidence
    ) %>%
    left_join(
      release_site_info,
      by = "ministry_code"
    )

  cases_joins %>%
    group_by(ministry_code) %>%
    mutate(
      post_intervention = case_when(
        date > start_release ~ TRUE,
        .default = FALSE
      ),
      .after = date
    ) %>%
    ungroup() %>%
    mutate(
      week_id = (year - min(year)) * 52 + epid_week,
      location_id = as.numeric(as.factor(ministry_code))
      ) %>%
    select(
      year,
      epid_week,
      ministry_code,
      cases,
      incidence,
      population,
      week_id,
      date,
      location_id,
      start_release,
      post_intervention
    ) %>%
  # there are some values that are missing where they hadn't yet recorded
  # dengue cases - we won't say that there were 0 cases, but we should drop
  # these observations. Since these are in long form we will only be removing
  # data for relevant rows
    filter(!is.na(cases))

}
