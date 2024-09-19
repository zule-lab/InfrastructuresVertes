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
  # model
  # prior predictive 
  # posterior predictive
  
  zar_brms(
    canopy_vsmpe,
    formula = per_can_s ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
    family = gaussian(),
    prior = c( 
      prior(normal(0, 0.3), class = "b"),
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
      prior(normal(0, 0.7), class = "b"),
      prior(normal(0, 0.7), class = "Intercept"),
      prior(exponential(1), class = "sigma")
    ),
    backend = 'cmdstanr',
    data = ecological_benefits %>% filter(city == "Trois-Rivières"),
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  zar_brms(
    fireflies_vsmpe,
    formula = firefly_presence ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
    family = bernoulli(),
    prior = c( 
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 1.5), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd")
    ),
    backend = 'cmdstanr',
    data = ecological_benefits %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  zar_brms(
    sr_vsmpe,
    formula = nSpecies ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
    family = poisson(),
    prior = c( 
      prior(normal(0, 0.2), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(normal(0, 0.1), class = "sd")
    ),
    backend = 'cmdstanr',
    data = ecological_benefits %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  zar_brms(
    sr_tr,
    formula = nSpecies ~ 1 + type,
    family = poisson(),
    prior = c( 
      prior(normal(0, 0.2), class = "b"),
      prior(normal(0, 0.5), class = "Intercept")
    ),
    backend = 'cmdstanr',
    data = ecological_benefits %>% filter(city == "Trois-Rivières"),
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  zar_brms(
    fg_vsmpe,
    formula = nFG ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
    family = poisson(),
    prior = c( 
      prior(normal(0, 0.2), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(normal(0, 0.1), class = "sd")
    ),
    backend = 'cmdstanr',
    data = ecological_benefits %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  zar_brms(
    fg_tr,
    formula = nFG ~ 1 + type,
    family = poisson(),
    prior = c( 
      prior(normal(0, 0.2), class = "b"),
      prior(normal(0, 0.5), class = "Intercept")
    ),
    backend = 'cmdstanr',
    data = ecological_benefits %>% filter(city == "Trois-Rivières"),
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  zar_brms(
    vc_vsmpe,
    formula = avg_complexity_s ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
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
    vc_tr,
    formula = avg_complexity_s ~ 1 + type,
    family = gaussian(),
    prior = c( 
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept")
    ),
    backend = 'cmdstanr',
    data = ecological_benefits %>% filter(city == "Trois-Rivières"),
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  zar_brms(
    pn_vsmpe,
    formula = percent_nat ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
    family = zero_one_inflated_beta(),
    prior = c( 
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd"),
      prior(gamma(0.01, 0.01), class = "phi"),
      prior(beta(1, 1), class = "zoi"),
      prior(beta(1, 1), class = "coi")
    ),
    backend = 'cmdstanr',
    data = ecological_benefits %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  zar_brms(
    pn_tr,
    formula = percent_nat ~ 1 + type,
    family = zero_one_inflated_beta(),
    prior = c( 
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(gamma(0.01, 0.01), class = "phi"),
      prior(beta(1, 1), class = "zoi"),
      prior(beta(1, 1), class = "coi")
    ),
    backend = 'cmdstanr',
    data = ecological_benefits %>% filter(city == "Trois-Rivières"),
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  
  zar_brms(
    pi_vsmpe,
    formula = percent_inv ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
    family = zero_one_inflated_beta(),
    prior = c( 
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(normal(0, 0.2), class = "sd"),
      prior(gamma(0.01, 0.01), class = "phi"),
      prior(beta(1, 1), class = "zoi"),
      prior(beta(1, 1), class = "coi")
    ),
    backend = 'cmdstanr',
    data = ecological_benefits %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  zar_brms(
    pi_tr,
    formula = percent_inv ~ 1 + type,
    family = zero_one_inflated_beta(),
    prior = c( 
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 0.5), class = "Intercept"),
      prior(gamma(0.01, 0.01), class = "phi"),
      prior(beta(1, 1), class = "zoi"),
      prior(beta(1, 1), class = "coi")
    ),
    backend = 'cmdstanr',
    data = ecological_benefits %>% filter(city == "Trois-Rivières"),
    chains = 4,
    iter = 1000,
    cores = 4
  ),
  
  # prior list
  tar_target(
    prior_model_list,
    list(canopy_vsmpe_brms_sample_prior, canopy_tr_brms_sample_prior, fireflies_vsmpe_brms_sample_prior, sr_vsmpe_brms_sample_prior, 
         sr_tr_brms_sample_prior, fg_vsmpe_brms_sample_prior, fg_tr_brms_sample_prior, vc_vsmpe_brms_sample_prior, vc_tr_brms_sample_prior,
          pn_vsmpe_brms_sample_prior, pn_tr_brms_sample_prior, pi_vsmpe_brms_sample_prior, pi_tr_brms_sample_prior) %>%
      setNames(., c('canopy_vsmpe_prior', 'canopy_tr_prior', 'fireflies_vsmpe_prior', 'sr_vsmpe_prior', 'sr_tr_prior',
                    'fg_vsmpe_prior', 'fg_tr_prior', 'vc_vsmpe_prior', 'vc_tr_prior', 'pn_vsmpe_prior', 'pn_tr_prior',
                    'pi_vsmpe_prior', 'pi_tr_prior'))
    
  ),
  
  # model list
  tar_target(
    model_list,
    list(canopy_vsmpe_brms_sample, canopy_tr_brms_sample, fireflies_vsmpe_brms_sample, sr_vsmpe_brms_sample, sr_tr_brms_sample,
         fg_vsmpe_brms_sample, fg_tr_brms_sample, vc_vsmpe_brms_sample, vc_tr_brms_sample, pn_vsmpe_brms_sample, pn_tr_brms_sample,
         pi_vsmpe_brms_sample, pi_tr_brms_sample) %>%
      setNames(., c('canopy_vsmpe', 'canopy_tr', 'fireflies_vsmpe', 'sr_vsmpe', 'sr_tr', 'fg_vsmpe', 'fg_tr',
                    'vc_vsmpe', 'vc_tr', 'pn_vsmpe', 'pn_tr', 'pi_vsmpe', 'pi_tr'))
  ),
  
  
  # prior checks 
  tar_render(
    prior_predictive,
    'graphics/diagnostics/prior_predictive.qmd'
  ),
  
  # model diagnostics
  tar_render(
    model_diagnostics,
    'graphics/diagnostics/model_diagnostics.qmd'
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
