plot_tree_abund <- function(trees_raw){
  
  trees <- trees_raw %>% 
    mutate(Genus = case_when(CommonName == 'Dead' ~ 'Dead',
                             .default = Genus),
           Genus = replace_na(Genus, 'Unknown'),
           Species = replace_na(Species, 'sp.'),
           scientific_name = paste(Genus, Species, sep = " "),
           type = case_when(str_detect(InfrastructureID, 'RV') == T ~ 'Ruelles vertes',
                            str_detect(InfrastructureID, 'SS') == T ~ 'Segments des rues',
                            str_detect(InfrastructureID, 'CON') == T ~ 'Ruelles grises'),
           city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                            str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières')) %>% 
    drop_na(DBH) 
  
  treecntruelle <- trees %>% 
    group_by(InfrastructureID) %>% 
    summarize(nTrees = n(), 
              type = first(type), 
              city = first(city)) %>% 
    group_by(type, city) %>% 
    summarize(meanabund = mean(nTrees))
  
  abund <- ggplot(treecntruelle) +
    geom_col(aes(y = meanabund, x = type, colour = city, fill = city), position = position_dodge()) + 
    scale_fill_manual(values = c("#6e948c", "#122c43")) + 
    scale_colour_manual(values = c("#6e948c", "#122c43")) + 
    theme_classic() + 
    labs(x = "", y = "Nombre moyen d'arbres par site", fill = "", colour = "", title = "1,469 arbres mesurés") + 
    theme(legend.position = 'top',
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          axis.title = element_text(size = 12))
  
  ggsave('graphics/treeabundancepersite.png', abund, width = 6, height = 8, units = 'in')
  
  
  treecnt <- trees %>% 
    group_by(type, city) %>% 
    tally() 

  
  plot <- ggplot(treecnt) +
    geom_col(aes(y = n, x = type, colour = city, fill = city), position = position_dodge()) + 
    scale_fill_manual(values = c("#6e948c", "#122c43")) + 
    scale_colour_manual(values = c("#6e948c", "#122c43")) + 
    theme_classic() + 
    labs(x = "", y = "Le nombre d'arbres au total", fill = "", colour = "", title = "1,469 arbres mesurés") + 
    theme(legend.position = 'top',
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          axis.title = element_text(size = 12))
  
  ggsave('graphics/treeabundance.png', plot, width = 6, height = 8, units = 'in')
  
  return(plot)
  
}