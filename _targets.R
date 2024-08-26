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
                  trees_clean, tree_traits)
  ),
  
  tar_target(
    census_data,
    calc_census()
  )
)
  

# Targets: plot -----------------------------------------------------------

targets_plot <- c(
  
  tar_target(
    figure_1,
    create_figure_1(study_rv, study_controls, quartiers)
  )
  
  #tar_target(
  #  veg_complexity,
  #  plot_veg_complexity(ecological_benefits)
  #),

  #tar_target(
  #  temp_mit,
  #  mit_temp(temp_dfs, tr_temp_dfs, study_rv, study_controls)
  #),
  #
  #tar_target(
  #  temp_plot,
  #  plot_temp(temp_mit)
  #),
  #
  #tar_target(
  #  tree_div,
  #  div_tree(trees_clean)
  #),
  #
  #tar_target(
  #  tree_species,
  #  plot_tree_species(tree_div)
  #),
  #
  #tar_target(
  #  temp_div_tradeoff,
  #  plot_temp_div(temp_plot, tree_species)
  #)
  #
  #tar_target(
  #  tree_abundance,
  #  plot_tree_abund(trees_clean)
  #),
)



# Targets: all ------------------------------------------------------------
# Automatically grab all the 'targets_*' lists above
lapply(grep('targets', ls(), value = TRUE), get)
