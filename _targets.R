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
                  can_cov_street, trees_clean, tree_traits, census_data)
  )
)


# Targets: analysis -------------------------------------------------------

targets_analysis <- c(
  
  # ch 3 analysis add date/ruelle as random effects for cooling, neighbourhood for all models 
  # mtl models adjust for language and income 
  # scale data
  # generative data? 
  # model
  # prior predictive 
  # posterior predictive
  
  
  zar_brms(
    canopy_vsmpe,
    formula = per_can_s ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
    family = gaussian(),
    prior = c( 
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    data = ecological_benefits %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  zar_brms(
    canopy_tr,
    formula = per_can_s ~ 1 + type,
    family = gaussian(),
    prior = c( 
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    data = ecological_benefits %>% filter(city == "Trois-Rivières"),
    chains = 4,
    iter = 1000,
    cores = 4
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
  )
)



# Targets: all ------------------------------------------------------------
# Automatically grab all the 'targets_*' lists above
lapply(grep('targets', ls(), value = TRUE), get)
