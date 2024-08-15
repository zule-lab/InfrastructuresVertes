calc_eco_bens <- function(fireflies_raw, ruelle_complexity_raw, street_complexity_raw, trees_clean){
  
  # veg abundance
  # veg diversity
  # firefly presence
  
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
