plot_tree_species <- function(tree_div, temp_plot){
    
  # isolate the ruelles that were highlighted in the temperature plot
  mtl_day <- temp_plot[[1]] %>% 
    group_by(plot_id) %>% 
    summarize(max = mean(mean_cooling)) %>% 
    slice_max(max, n = 3)
  
  tr_day <- temp_plot[[3]] %>% 
    group_by(plot_id) %>% 
    summarize(max = mean(mean_cooling)) %>% 
    slice_max(max, n = 3)
    
  max_temps <- rbind(mtl_day, tr_day)
  
  # pull out type of GI and city
  trees <- tree_div %>% 
  mutate(type = case_when(str_detect(InfrastructureID, 'RV') == T ~ 'Ruelles Vertes',
                          str_detect(InfrastructureID, 'SS') == T ~ 'Segments des Rues',
                          str_detect(InfrastructureID, 'CON') == T ~ 'Ruelles Traditionelles'),
         city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                          str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières'),
         InfrastructureID = str_replace(InfrastructureID, "-0", "-"))
  
  # join temps and tree div 
  full <- left_join(trees, max_temps, by = join_by("InfrastructureID" == "plot_id")) 
  
  
  mtl <- ggplot(full %>% filter(city == "Villeray-Saint Michel-Parc Extension"), 
         aes(x = type, y = SpeciesRichness)) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_point(aes(colour = max), position=position_jitterdodge(), size = 3) +
    theme_classic() + 
    labs(x = "", colour = "", y = "Le nombre d'espèces d'arbres", title = "Villeray-Saint Michel-Parc Extension") + 
    theme(legend.position = 'none')
  
  tr <- ggplot(full %>% filter(city == "Trois-Rivières"), 
         aes(x = type, y = SpeciesRichness)) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_point(aes(colour = max), position=position_jitterdodge(), size = 3) +
    theme_classic() + 
    labs(x = "", colour = "", y = "Le nombre d'espèces d'arbres", title = "Trois-Rivières") + 
    theme(legend.position = 'none')
  
  div <- mtl | tr
    
  ggsave('graphics/treespecies.png', div, width = 12, height = 8, units = 'in')
    
  return(plot)
    
}
