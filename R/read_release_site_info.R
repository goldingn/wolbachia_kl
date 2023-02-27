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
    filter(site != "W20_REJANG") %>%
    mutate(
      stop_release = case_when(
        stop_release == "ongoing" ~ "22/26",
        .default = stop_release
      ),
    last_monitoring = case_when(
      is.na(last_monitoring) ~ "22/26",
      .default = last_monitoring
    )
    ) %>%
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
    ) %>%
    rename(
      fw_release = freq_wolb_fw_for_the_intervention_period,
      fw_monitoring = freqwolb_post_intervention
    ) %>%
    mutate(
      fw_prerelease = 0,
      fw_postmonitoring = NA,
      .after = fw_monitoring
    )

}
