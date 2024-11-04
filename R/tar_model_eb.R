tar_model_eb <- function(){
  
  c(
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
      formula = number_trees_nat | trials(total_trees) ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
      family = binomial(),
      prior = c( 
        prior(normal(0, 0.5), class = "b"),
        prior(normal(0, 0.5), class = "Intercept"),
        prior(normal(0, 0.2), class = "sd")
      ),
      backend = 'cmdstanr',
      data = ecological_benefits %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
      chains = 4,
      iter = 1000,
      cores = 4
    ),
    
    zar_brms(
      pn_tr,
      formula = number_trees_nat | trials(total_trees) ~ 1 + type,
      family = binomial(),
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
      pi_vsmpe,
      formula = number_trees_inv | trials(total_trees) ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
      family = binomial(),
      prior = c( 
        prior(normal(0, 0.5), class = "b"),
        prior(normal(0, 0.5), class = "Intercept"),
        prior(normal(0, 0.2), class = "sd")
      ),
      backend = 'cmdstanr',
      data = ecological_benefits %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
      chains = 4,
      iter = 1000,
      cores = 4
    ),
    
    zar_brms(
      pi_tr,
      formula = number_trees_inv | trials(total_trees) ~ 1 + type,
      family = binomial(),
      prior = c( 
        prior(normal(0, 0.5), class = "b"),
        prior(normal(0, 0.5), class = "Intercept"),
        prior(gamma(0.01, 0.01), class = "phi")
      ),
      backend = 'cmdstanr',
      data = ecological_benefits %>% filter(city == "Trois-Rivières"),
      chains = 4,
      iter = 1000,
      cores = 4
    )
    
  )
  
}
    