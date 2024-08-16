calc_eco_bens <- function( study_rv, study_controls,  can_cov_street, fireflies_raw, 
                          ruelle_complexity_raw, street_complexity_raw, trees_clean){
  
  # canopy cover
  can_cov_street <- can_cov_street %>% 
    mutate(CODE_ARR = case_when(str_detect(Name, 'VSMPE') == T ~ 'VSMPE', 
                                str_detect(Name, 'TR') == T ~ 'TR'),
           Q_socio = NA) %>% 
    rename(RUELLE_CODE = "Name",
           ruelle_area = "street_area")
  
  study_rv <- study_rv %>% select(-RUELLE_ID)
  study_controls <- study_controls %>% select(-RUELLE_ID)
  
  cancov <- rbind(can_cov_street, study_rv, study_controls)
  
  # firefly presence/absence
  
  
  
  # veg abundance
  
  
  # veg diversity
 
  
  # veg complexity 
  vegtotal <- ruelle_complexity_raw %>% 
    select(-c(Avarage.of.layers, X)) %>% 
    group_by(InfrastructureID) %>% 
    summarize(total_layers = sum(Layers),
              npoints = n(), 
              avg_complexity = total_layers/npoints) %>% 
    mutate(type = case_when(str_detect(InfrastructureID, 'RV') == T ~ 'Ruelles Vertes',
                            str_detect(InfrastructureID, 'CON') == T ~ 'Ruelles Traditionelles'),
           city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                            str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières'))
  
  streettotal <- street_complexity_raw %>% 
    select(-c(Comments, X)) %>% 
    group_by(InfrastructureID) %>%
    summarize(total_layers = sum(Layers),
              npoints = n(), 
              avg_complexity = total_layers/npoints) %>% 
    mutate(type = case_when(str_detect(InfrastructureID, 'SS') == T ~ 'Segments des Rues'),
           city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                            str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières'))
  
  complexity <- rbind(vegtotal, streettotal)
  
  # invasives/natives
  
  
}
