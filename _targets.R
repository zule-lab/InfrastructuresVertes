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
    ecological_benefits,
    calc_eco_bens(study_rv, study_controls,  can_cov_street, fireflies_raw,
                  tree_traits, ruelle_description, ruelle_complexity_raw, 
                  street_complexity_raw, trees_clean, quartiers)
  ),
  
  tar_target(
    ecosystem_services,
    calc_eco_serv(temp_dfs, tr_temp_dfs, study_rv, study_controls,
                  can_cov_street, trees_clean, tree_traits)
  ),
  
  tar_target(
    census_data,
    calc_census(da_raw, census_raw, study_rv, study_controls)
  )
)


# Targets: analysis -------------------------------------------------------


  

# Targets: plot -----------------------------------------------------------

targets_plot <- c(
  
  tar_target(
    mtl_dags,
    create_mtl_dags()
  ),
  
  tar_target(
    study_fig,
    create_study_fig(study_rv, study_controls, quartiers)
  )
)



# Targets: all ------------------------------------------------------------
# Automatically grab all the 'targets_*' lists above
lapply(grep('targets', ls(), value = TRUE), get)
