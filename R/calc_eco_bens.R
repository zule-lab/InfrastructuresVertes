calc_eco_bens <- function( study_rv, study_controls, can_cov_street, fireflies_raw, 
                          ruelle_complexity_raw, street_complexity_raw, trees_clean){
  
  # canopy cover
  can_cov_street <- can_cov_street %>% 
    mutate(CODE_ARR = case_when(str_detect(Name, 'VSMPE') == T ~ 'VSMPE', 
                                str_detect(Name, 'TR') == T ~ 'TR'),
           Q_socio = NA) %>% 
    rename(InfrastructureID = "Name",
           ruelle_area = "street_area")
  
  study_rv <- study_rv %>% select(-RUELLE_ID) %>% rename(InfrastructureID = "RUELLE_CODE")
  study_controls <- study_controls %>% select(-RUELLE_ID) %>% rename(InfrastructureID = "RUELLE_CODE")
  
  cancov <- rbind(can_cov_street, study_rv, study_controls) %>% select(-Q_socio)
  
  
  # firefly presence/absence
  fireflies_can <- fireflies_raw %>% 
    group_by(Infrastructure.ID) %>% 
    summarize(fireflies = sum(FirefliesObserved.... )) %>% 
    rename(InfrastructureID = "Infrastructure.ID") %>% 
    mutate(firefly_presence = case_when(fireflies == 0 ~ 0, 
                                        fireflies != 0 ~ 1)) %>% 
    right_join(., cancov, by = "InfrastructureID")
  
  
  # tree abundance (veg abundance?)
  can_fireflies_abund <- trees_clean %>% 
    mutate(Genus = case_when(CommonName == 'Dead' ~ 'Dead',
                             .default = Genus),
           Genus = replace_na(Genus, 'Unknown'),
           Species = replace_na(Species, 'sp.'),
           scientific_name = paste(Genus, Species, sep = " "),
           type = case_when(str_detect(InfrastructureID, 'RV') == T ~ 'Ruelles Vertes',
                            str_detect(InfrastructureID, 'SS') == T ~ 'Segments des Rues',
                            str_detect(InfrastructureID, 'CON') == T ~ 'Ruelles Traditionelles'),
           city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                            str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières')) %>% 
    group_by(InfrastructureID) %>% 
    summarize(nTrees = n(), 
              type = first(type), 
              city = first(city)) %>% 
    right_join(., fireflies_can, by = "InfrastructureID") %>%
    mutate(nTrees = if_else(is.na(nTrees), 0, nTrees))
  
  # tree diversity
  
  
  
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
