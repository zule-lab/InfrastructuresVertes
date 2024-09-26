tar_model_es <- function(){
  
  c(
    zar_brms(
      cooling_vsmpe,
      formula = cooling_s ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio) + (1 | date) + (1 | tod) + (1 | InfrastructureID),
      family = gaussian(),
      prior = c( 
        prior(normal(0, 0.5), class = "b"),
        prior(normal(0, 0.5), class = "Intercept"),
        prior(normal(0, 0.2), class = "sd"),
        prior(exponential(1), class = "sigma")
      ),
      backend = 'cmdstanr',
      data = ecosystem_services[[1]] %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
      chains = 4,
      iter = 1000,
      cores = 4
    ),
    
    zar_brms(
      cooling_tr,
      formula = cooling_s ~ 1 + type + (1 | date) + (1 | tod) + (1 | InfrastructureID),
      family = gaussian(),
      prior = c( 
        prior(normal(0, 0.5), class = "b"),
        prior(normal(0, 0.5), class = "Intercept"),
        prior(normal(0, 0.2), class = "sd")
      ),
      backend = 'cmdstanr',
      data = ecosystem_services[[1]] %>% filter(city == "Trois-Rivières"),
      chains = 4,
      iter = 1000,
      cores = 4
    ),
    
    zar_brms(
      ta_vsmpe,
      formula = nTrees ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
      family = poisson(),
      prior = c( 
        prior(normal(0, 0.2), class = "b"),
        prior(normal(0, 0.5), class = "Intercept"),
        prior(normal(0, 0.1), class = "sd")
      ),
      backend = 'cmdstanr',
      data = ecosystem_services[[2]] %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
      chains = 4,
      iter = 1000,
      cores = 4
    ),
    
    zar_brms(
      ta_tr,
      formula = nTrees ~ 1 + type,
      family = poisson(),
      prior = c( 
        prior(normal(0, 0.2), class = "b"),
        prior(normal(0, 0.5), class = "Intercept")
      ),
      backend = 'cmdstanr',
      data = ecosystem_services[[2]] %>% filter(city == "Trois-Rivières"),
      chains = 4,
      iter = 1000,
      cores = 4
    ),
    
    zar_brms(
      dbh_vsmpe,
      formula = meanDBH_s ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
      family = gaussian(),
      prior = c( 
        prior(normal(0, 0.3), class = "b"),
        prior(normal(0, 0.5), class = "Intercept"),
        prior(normal(0, 0.2), class = "sd"),
        prior(exponential(1), class = "sigma")
      ),
      backend = 'cmdstanr',
      data = ecosystem_services[[2]] %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
      chains = 4,
      iter = 1000,
      cores = 4
    ),
    
    zar_brms(
      dbh_tr,
      formula = meanDBH_s ~ 1 + type,
      family = gaussian(),
      prior = c( 
        prior(normal(0, 0.7), class = "b"),
        prior(normal(0, 0.7), class = "Intercept"),
        prior(exponential(1), class = "sigma")
      ),
      backend = 'cmdstanr',
      data = ecosystem_services[[2]] %>% filter(city == "Trois-Rivières"),
      chains = 4,
      iter = 1000,
      cores = 4
    ),
    
    zar_brms(
      hgt_vsmpe,
      formula = mean_pot_hgt_s ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
      family = gaussian(),
      prior = c( 
        prior(normal(0, 0.3), class = "b"),
        prior(normal(0, 0.5), class = "Intercept"),
        prior(normal(0, 0.2), class = "sd"),
        prior(exponential(1), class = "sigma")
      ),
      backend = 'cmdstanr',
      data = ecosystem_services[[2]] %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
      chains = 4,
      iter = 1000,
      cores = 4
    ),
    
    zar_brms(
      hgt_tr,
      formula = mean_pot_hgt_s ~ 1 + type,
      family = gaussian(),
      prior = c( 
        prior(normal(0, 0.7), class = "b"),
        prior(normal(0, 0.7), class = "Intercept"),
        prior(exponential(1), class = "sigma")
      ),
      backend = 'cmdstanr',
      data = ecosystem_services[[2]] %>% filter(city == "Trois-Rivières"),
      chains = 4,
      iter = 1000,
      cores = 4
    ),
    
    zar_brms(
      pf_vsmpe,
      formula = prop_showy ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
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
      data = ecosystem_services[[2]] %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
      chains = 4,
      iter = 1000,
      cores = 4
    ),
    
    zar_brms(
      pf_tr,
      formula = prop_showy ~ 1 + type,
      family = zero_one_inflated_beta(),
      prior = c( 
        prior(normal(0, 0.5), class = "b"),
        prior(normal(0, 0.5), class = "Intercept"),
        prior(gamma(0.01, 0.01), class = "phi"),
        prior(beta(1, 1), class = "zoi"),
        prior(beta(1, 1), class = "coi")
      ),
      backend = 'cmdstanr',
      data = ecosystem_services[[2]] %>% filter(city == "Trois-Rivières"),
      chains = 4,
      iter = 1000,
      cores = 4
    )
    
  )
  
  
}