plot_tree_species <- function(tree_div){
    
  # pull out type of GI and city
  trees <- tree_div %>% 
  mutate(InfrastructureID = str_replace(InfrastructureID, "RV-SS", "SS"),
         type = case_when(str_detect(InfrastructureID, 'RV') == T ~ 'Ruelles vertes',
                          str_detect(InfrastructureID, 'SS') == T ~ 'Segments des rues',
                          str_detect(InfrastructureID, 'CON') == T ~ 'Ruelles grises'),
         city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                          str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières'),
         InfrastructureID = str_replace(InfrastructureID, "-0", "-"))
  
  
  div <- ggplot(trees, aes(x = type, y = SpeciesRichness, colour = city)) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_point(size = 2, position = position_jitterdodge()) +
    scale_colour_manual(values = c("#6e948c", "#122c43")) + 
    theme_classic() + 
    labs(x = "", colour = "", y = "Le nombre d'espèces d'arbres") + 
    theme(legend.position = 'top',
          axis.text = element_text(size = 16, color = 'black'),
          axis.title = element_text(size = 16), 
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16))
  

    
  ggsave('graphics/treespecies.png', div, width = 12, height = 8, units = 'in')
    
  return(trees)
    
}
