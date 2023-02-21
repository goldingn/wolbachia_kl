# extract release data

library(readxl)
library(tidyverse)
released_raw <- read_excel(
  path = "data/raw/Manuscript one modeling dataset additional 13Sep2019.xlsx",
  sheet = 1,
  .name_repair = janitor::make_clean_names
  ) %>%
  tibble::rowid_to_column()

dat_release <- released_raw %>%
  select(
    rowid,
    year,
    epid_week,
    ends_with(c("_release","_released"))
  )

dat_release %>%
  select(rowid,
         year,
         epid_week,
         section_7_pkns_release) %>%
  filter(!is.na(section_7_pkns_release)) %>%
  slice_head(n = 1)


get_first_release <- function(data, column){

  column_name <- rlang::as_string(rlang::enexpr(column))
  data %>%
    select(rowid,
           year,
           epid_week,
           {{ column }}) %>%
    filter(!is.na( {{ column }} )) %>%
    slice_head(n = 1) %>%
    mutate(colname = column_name) %>%
    select(- {{ column }})
}

get_first_release(dat_release, section_7_pkns_release)

release_names <- names(dat_release) %>%
  str_subset("_releas")

release_dates_and_names <- syms(release_names) %>%
  map_dfr(get_first_release,
          data = dat_release)

release_codes_raw <- read_excel(
  path = "data/raw/Manuscript one modeling dataset additional 13Sep2019.xlsx",
  sheet = 3,
  range = "K2:BG3"
)

release_codes <- release_codes_raw %>%
  pivot_longer(everything()) %>%
  filter(!is.na(value))

release_codes
release_dates_and_names

library(stringdist)

min_string_dist <- function(
    to_match,
    in_set
){
  dist <- stringdist::stringdist(to_match, in_set)
  in_set[which.min(dist)]
}


release_dates_and_names$colname[which.min(stringdist::stringdist(release_codes$value[1], release_dates_and_names$colname))]

min_string_dist(release_dates_and_names$colname[1], release_codes$value)
stringdist(release_dates_and_names$colname[1], release_codes$value)
