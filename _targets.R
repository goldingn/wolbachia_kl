## Load packages
source("./packages.R")

## Load all R files
lapply(list.files("./R", full.names = TRUE), source)

tar_plan(
  tar_file(wolbachia_path, here("data/raw/2023_02_16_final_data.xlsx")),
  # keep the
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
  # TODO
  # gg_cluster_figure = plot_ts_cluster_figure(wolbachia_2023),
  gg_intervention = plot_ts_intervention(wolbachia_2023),
  tar_quarto(clusters, "doc/clusters.qmd"),
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
  ### ---
  # add in a second analysis where we don't use post_intervention
  # instead we use wolbachia_freq, which is 0-1
  # 0 pre release, then some number during release, then the post number
  # - in this case we are dropping Rejang
  # make sure to make the ongoing release
  #
  ### ---

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

  tar_quarto(eda, "doc/eda.qmd"),

  ### - using fw term ----

  tar_file(draws_fw_path, here("analysis-results/draws_fw.rds")),
  tar_file(means_fw_path, here("analysis-results/means_fw.rds")),
  tar_file(wolbachia_results_fw_path, here("analysis-results/wolbachia_2023_model_results_fw.rds")),
  tar_file(parameter_summary_fw_path, here("analysis-results/parameter_summary_fw.rds")),
  tar_file(perc_change_draws_fw_vec_path, here("analysis-results/perc_change_draws_fw_vec.rds")),
  draws_fw = read_rds(draws_fw_path),
  means_fw = read_rds(means_fw_path),

  parameter_summary_fw = read_rds(parameter_summary_fw_path),
  perc_change_draws_fw_vec = read_rds(perc_change_draws_fw_vec_path),

  ### - some post processing of greta fw
  wolbachia_2023_model_results_fw = add_greta_model_summaries_fw(
    means_fw,
    wolbachia_2023
  ),
  ###

  tar_file(wolbachia_2023_model_results_fw_path,{
    write_csv_return_path(wolbachia_2023_model_results_fw,
                          "data/clean/wolbachia_2023_model_results_fw.csv")
  }),
  dharma_fw = greta_dharma(
    wolbachia_2023_model_results_fw,
    means_fw
  ),

  tar_quarto(eda_fw, "doc/eda-fw.qmd"),
  ### - end using fw term

)
