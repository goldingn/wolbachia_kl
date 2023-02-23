## Load packages
source("./packages.R")

## Load all R files
lapply(list.files("./R", full.names = TRUE), source)

tar_plan(
  tar_file(wolbachia_path, here("data/raw/2023_02_16_final_data.xlsx")),
  release_site_info = read_release_site_info(wolbachia_path),
  release_control_sites = read_release_control_sites(wolbachia_path),
  cases_lookup = read_cases_lookup(wolbachia_path),
  cases = read_cases(wolbachia_path),
  incidence_lookup = read_incidence_lookup(wolbachia_path),
  # incidence_lookup and cases_lookup are identical
  incidence_lookup_is_cases_lookup = identical(cases_lookup, incidence_lookup),
  incidence = read_incidence(wolbachia_path),
  wolbachia_2023 = tidy_wolbachia(
    cases = cases,
    incidence = incidence,
    release_site_info = release_site_info,
    release_control_sites = release_control_sites,
    cases_lookup = cases_lookup
  ),
  gg_intervention = plot_ts_intervention(wolbachia_2023),
  # don't run this anymore as we run into memory issues that are
  # hard to debug - we will run this in the scripts/greta-analysis.R
  # and then save the outputs
  # greta_analysis = analysis_greta(
  #   wolbachia_2023,
  #   n_warmup = 2000,
  #   n_samples = 2000,
  #   n_extra_draws = 2000,
  #   Lmin = 30,
  #   Lmax = 40
  #   ),

  tar_file(draws_path, here("analysis-results/draws.rds")),
  tar_file(means_path, here("analysis-results/means.rds")),
  tar_file(wolbachia_results_path, here("analysis-results/wolbachia_2023_model_results.rds")),
  tar_file(parameter_summary_path, here("analysis-results/parameter_summary.rds")),
  tar_file(perc_change_draws_vec_path, here("analysis-results/perc_change_draws_vec.rds")),
  draws = read_rds(draws_path),
  means = read_rds(means_path),
  wolbachia_2023_model_results = read_rds(wolbachia_results_path),
  parameter_summary = read_rds(parameter_summary_path),
  perc_change_draws_vec = read_rds(perc_change_draws_vec_path),

  tar_file(wolbachia_2023_model_results_path,{
    write_csv_return_path(wolbachia_2023_model_results,
                          "data/clean/wolbachia_2023_model_results.csv")
  }),

  dharma = greta_dharma(
    wolbachia_2023,
    means
    ),

  tar_quarto(eda, "doc/eda.qmd")

)
