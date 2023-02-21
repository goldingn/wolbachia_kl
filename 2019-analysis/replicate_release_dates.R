releases <- readr::read_csv(here::here("data/raw/release_dates_raw_raw.csv"))

library(lubridate)

release_dat <- releases %>%
  mutate(
    release_date = as.Date("0000-01-01") +
      years(release_year) +
      weeks(release_week - 1)
  ) %>%
  select(site,
         release_date)

release_dat

write_csv(release_dat, here::here("data/raw/release_dates.csv"))
