plot_per_invasive <- function(trees_clean, tree_traits){
  
  per_inv <- left_join(trees_clean, tree_traits, by = join_by('scientific_name' == 'latin.name')) %>% 
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
    mutate(type = case_when(str_detect(InfrastructureID, 'RV') == T ~ 'Ruelles vertes',
                            str_detect(InfrastructureID, 'SS') == T ~ 'Segments des rues',
                            str_detect(InfrastructureID, 'CON') == T ~ 'Ruelles grises'),
           city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                            str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières')) %>% 
    filter(native_invasive != 'nonnat')
  
  labls <- c("Espèces invasives", "Espèces indigènes")
  names(labls) <- c("inv", "nat")
  
  
  plot <- ggplot(per_inv, aes(x = type, y = percent, colour = city)) + 
    geom_boxplot() + 
    geom_point(position=position_jitterdodge()) + 
    scale_colour_manual(values = c("#6e948c", "#122c43")) +
    theme_classic() + 
    labs(colour = "", x = "", y = "Proportion d'arbres") + 
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=1),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12)) + 
    facet_wrap(~native_invasive, labeller = labeller(native_invasive = labls))
  
  ggsave('graphics/native_invasive.png', plot, height = 10, width = 12, units = 'in')
  
}