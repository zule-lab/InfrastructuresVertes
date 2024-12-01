# === Targets -------------------------------------------------------------
# Framework by Alec L. Robitaille


# Source ------------------------------------------------------------------
library(targets)
tar_source('R')



# Options -----------------------------------------------------------------
# Targets
tar_option_set(format = 'qs')
options(timeout=100)




# Renv --------------------------------------------------------------------
activate()
snapshot()
restore()


# Targets: data prep ------------------------------------------------------

targets_data <- c(
  
  # target list for reading all data files
  tar_read_files(),
  
  # target list for calculating canopy cover
  tar_canopy(),
  
  tar_target(
    study_rv,
    select_study(can_cov_rv_bind, quartiers)
  ),
  
  tar_target(
    study_controls,
    select_controls(can_cov_controls_bind, quartiers)
  ),
  
  tar_target(
    trees_clean,
    clean_trees(trees_raw)
  ),
  
  tar_target(
    census_data,
    calc_census(da_raw, census_raw, study_rv, study_controls, can_cov_street)
  ),
  
  tar_target(
    ecological_benefits,
    calc_eco_bens(study_rv, study_controls,  can_cov_street, fireflies_raw,
                  tree_traits, ruelle_description, ruelle_complexity_raw, 
                  street_complexity_raw, trees_clean, quartiers, census_data)
  ),
  
  tar_target(
    ecosystem_services,
    calc_eco_serv(temp_dfs, tr_temp_dfs, study_rv, study_controls,
                  can_cov_street, trees_clean, tree_traits, census_data,
                  quartiers)
  )
)


# Targets: analysis -------------------------------------------------------

targets_analysis <- c(
  
  # target list of all ecological benefit models 
  tar_model_eb(), 
  
  # target list of all ecosystem service models 
  tar_model_es(),
  
  # prior list
  tar_target(
    prior_model_list,
    list(canopy_vsmpe_brms_sample_prior, canopy_tr_brms_sample_prior, fireflies_vsmpe_brms_sample_prior, sr_vsmpe_brms_sample_prior,
         sr_tr_brms_sample_prior, fg_vsmpe_brms_sample_prior, fg_tr_brms_sample_prior, vc_vsmpe_brms_sample_prior, vc_tr_brms_sample_prior,
         pn_vsmpe_brms_sample_prior, pn_tr_brms_sample_prior, pi_vsmpe_brms_sample_prior, pi_tr_brms_sample_prior, 
         temp_vsmpe_brms_sample_prior, temp_tr_brms_sample_prior, 
         ta_vsmpe_brms_sample_prior, ta_tr_brms_sample_prior, dbh_vsmpe_brms_sample_prior, dbh_tr_brms_sample_prior, 
         hgt_vsmpe_brms_sample_prior, hgt_tr_brms_sample_prior, pf_vsmpe_brms_sample_prior, pf_tr_brms_sample_prior) %>%
      setNames(., c('canopy_vsmpe_prior', 'canopy_tr_prior', 'fireflies_vsmpe_prior', 'sr_vsmpe_prior', 'sr_tr_prior',
                    'fg_vsmpe_prior', 'fg_tr_prior', 'vc_vsmpe_prior', 'vc_tr_prior', 'pn_vsmpe_prior', 'pn_tr_prior',
                    'pi_vsmpe_prior', 'pi_tr_prior', 'temp_vsmpe_prior', 'temp_tr_prior', 
                    'ta_vsmpe_prior', 'ta_tr_prior', 'dbh_vsmpe_prior', 'dbh_tr_prior', 'hgt_vsmpe_prior', 'hgt_tr_prior', 'pf_vsmpe_prior', 'pf_tr_prior'))
    
  ),
  
  # model list
  tar_target(
    model_list,
    list(canopy_vsmpe_brms_sample, canopy_tr_brms_sample, fireflies_vsmpe_brms_sample, sr_vsmpe_brms_sample, sr_tr_brms_sample,
         fg_vsmpe_brms_sample, fg_tr_brms_sample, vc_vsmpe_brms_sample, vc_tr_brms_sample, pn_vsmpe_brms_sample, pn_tr_brms_sample,
         pi_vsmpe_brms_sample, pi_tr_brms_sample, temp_vsmpe_brms_sample, temp_tr_brms_sample, ta_vsmpe_brms_sample, ta_tr_brms_sample, 
         dbh_vsmpe_brms_sample, dbh_tr_brms_sample, hgt_vsmpe_brms_sample, hgt_tr_brms_sample, pf_vsmpe_brms_sample, pf_tr_brms_sample) %>%
      setNames(., c('canopy_vsmpe', 'canopy_tr', 'fireflies_vsmpe', 'sr_vsmpe', 'sr_tr', 'fg_vsmpe', 'fg_tr',
                    'vc_vsmpe', 'vc_tr', 'pn_vsmpe', 'pn_tr', 'pi_vsmpe', 'pi_tr', 'temp_vsmpe', 'temp_tr', 
                    'ta_vsmpe', 'ta_tr', 'dbh_vsmpe', 'dbh_tr','hgt_vsmpe', 'hgt_tr', 'pf_vsmpe', 'pf_tr'))
  ),
  
  tar_target(
    mod_tables,
    results_tables(model_list, ecological_benefits, ecosystem_services)
  )
  
  
)



# Targets: plot -----------------------------------------------------------

targets_plot <- c(
  
  tar_target(
    mtl_dags,
    create_mtl_dags()
  ),
  
  tar_target(
    study_fig,
    create_study_fig(study_rv, study_controls, quartiers)
  ),
  
  tar_render(
    supplementary,
    'graphics/supplementary.qmd'
  )
)



# Targets: all ------------------------------------------------------------
# Automatically grab all the 'targets_*' lists above
lapply(grep('targets', ls(), value = TRUE), get)
