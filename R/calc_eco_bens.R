calc_eco_bens <- function(study_rv, study_controls,  can_cov_street, fireflies_raw,
                          tree_traits, ruelle_description, ruelle_complexity_raw, 
                          street_complexity_raw, trees_clean, quartiers){
  
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
  
  
  # tree abundance
  can_fireflies_abund <- trees_clean %>% 
    mutate(Genus = case_when(CommonName == 'Dead' ~ 'Dead',
                             .default = Genus),
           Genus = replace_na(Genus, 'Unknown'),
           Species = replace_na(Species, 'sp.'),
           scientific_name = paste(Genus, Species, sep = " ")) %>% 
    group_by(InfrastructureID) %>% 
    summarize(nTrees = n(), 
              type = first(type), 
              city = first(city)) %>% 
    right_join(., fireflies_can, by = "InfrastructureID") %>%
    mutate(nTrees = if_else(is.na(nTrees), 0, nTrees))
  
  
  # tree diversity - functional groups & SR
  can_fireflies_abund_div <- left_join(trees_clean, tree_traits, by = join_by('scientific_name' == 'latin.name')) %>% 
    group_by(InfrastructureID) %>% 
    reframe(scientific_name = unique(scientific_name),
              functional_group = first(FunctionalGroup)) %>% 
    group_by(InfrastructureID) %>% 
    mutate(nSpecies = n()) %>% 
    reframe(functional_group = unique(functional_group),
            nSpecies = first(nSpecies)) %>% 
    group_by(InfrastructureID) %>% 
    mutate(nFG = n()) %>% 
    right_join(., can_fireflies_abund) %>% 
    mutate(type = case_when(str_detect(InfrastructureID, 'RV') == T ~ 'Ruelles Vertes',
                            str_detect(InfrastructureID, 'SS') == T ~ 'Segments des Rues',
                            str_detect(InfrastructureID, 'CON') == T ~ 'Ruelles Traditionelles'),
           city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                            str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières'),
           nSpecies = replace_na(nSpecies, 0),
           nFG = replace_na(nFG, 0),
           InfrastructureID = str_trim(InfrastructureID))
  
  
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
  
  can_fireflies_abund_div_complex <- rbind(vegtotal, streettotal) %>% 
    right_join(., can_fireflies_abund_div, by = "InfrastructureID", suffix = c(".y", ""))
  
  
  # impervious surface
  can_fireflies_abund_div_complex_imp <- ruelle_description %>% 
    group_by(InfrastructureID) %>% 
    reframe(permeable = unique(Permeable.Impermeable),
            per_permeable = unique(PercentagePermeability)) %>% 
    mutate(per_permeable = str_replace(per_permeable, "%", ""),
           per_permeable = as.double(per_permeable),
           per_permeable = replace_na(per_permeable, 0)) %>% 
    slice(-1L) %>% 
    right_join(., can_fireflies_abund_div_complex)
  
  
  # invasives/natives
  can_fireflies_abund_div_complex_imp_inv <- left_join(trees_clean, tree_traits, by = join_by('scientific_name' == 'latin.name')) %>% 
    group_by(InfrastructureID, scientific_name) %>% 
    summarize(n = n(), 
            Native_SLL = first(Native_SLL), 
            Native_ETF = first(Native_ETF), 
            Invasive = first(Invasive)) %>% 
    mutate(native_invasive = case_when(Native_SLL == 1 ~ 'nat',
                                       Native_ETF == 1 ~ 'nat',
                                       Native_SLL == 0 & Native_ETF == 0 & Invasive == 0 ~ 'nonnat',
                                       Invasive == 1 ~ 'inv')) %>% 
    group_by(InfrastructureID) %>% 
    mutate(total_trees = sum(n)) %>%
    group_by(InfrastructureID, native_invasive) %>% 
    summarize(total_trees = first(total_trees), 
              group_trees = sum(n),
              percent = group_trees/total_trees) %>% 
    pivot_wider(id_cols = InfrastructureID, names_from = native_invasive, values_from = percent, names_prefix = "percent_") %>% 
    rename(percent_unk = percent_NA) %>% 
    right_join(., can_fireflies_abund_div_complex_imp)
  
  
  # neighbourhood
  nhood <- can_fireflies_abund_div_complex_imp_inv %>% 
    st_as_sf() %>% 
    st_transform(st_crs(quartiers)) %>% 
    st_join(., quartiers, by= st_intersection, left = T) %>%
    select(-c(type.y, city.y, CODE_ARR, id, Arrondisse, Abrev, nbr_RUI, Table)) %>% 
    # one ruelle slightly crosses over into Saint-Michel so returns duplicate but it isn't truly in SM
    filter(!(InfrastructureID == "RV-VSMPE-20" & Q_socio == "Saint-Michel"))
  
  
  return(nhood)
  
}
