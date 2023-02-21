## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

tar_plan(
  tar_file(wolbachia_path, here("data/raw/2023_02_16_final_data.xlsx")),
  release_site_info = read_release_site_info(wolbachia_path),
  release_control_sites = read_release_control_sites(wolbachia_path),
  cases_lookup = read_cases_lookup(wolbachia_path),
  cases = read_cases(wolbachia_path),
  incidence_lookup = read_incidence_lookup(wolbachia_path),
  incidence = read_incidence(wolbachia_path)

)
