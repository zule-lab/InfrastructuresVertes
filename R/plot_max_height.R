plot_max_height <- function(trees_clean, tree_traits){
  
  df <- left_join(trees_clean, tree_traits, by = join_by('scientific_name' == 'latin.name')) %>% 
    group_by(InfrastructureID) %>% 
    summarize(mean_max_height = mean(maximum.height, na.rm=T)) %>% 
    mutate(type = case_when(str_detect(InfrastructureID, 'RV') == T ~ 'Ruelles vertes',
                            str_detect(InfrastructureID, 'SS') == T ~ 'Segments des rues',
                            str_detect(InfrastructureID, 'CON') == T ~ 'Ruelles grises'),
           city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                            str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières'))
  
  
  plot <- ggplot(df, aes(x = type, y = mean_max_height, colour = city)) + 
    geom_boxplot() + 
    geom_point(position=position_jitterdodge()) + 
    scale_colour_manual(values = c("#6e948c", "#122c43")) +
    theme_classic() + 
    labs(colour = "", x = "", y = "Hauteur maximale moyenne des arbres (m)") + 
    theme(legend.position = "top",
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  
  
  ggsave('graphics/maxheight.png', plot, height = 10, width = 12, units = 'in')
  
  
}