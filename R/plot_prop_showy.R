plot_prop_showy <- function(trees_clean, tree_traits){
  
  
  df <- left_join(trees_clean, tree_traits, by = join_by('scientific_name' == 'latin.name')) %>% 
    group_by(InfrastructureID, scientific_name) %>% 
    summarize(n = n(), 
              flowering = first(flowering)) %>% 
    group_by(InfrastructureID) %>% 
    mutate(total_trees = sum(n)) %>%
    group_by(InfrastructureID, flowering) %>% 
    summarize(total_trees = first(total_trees), 
              group_trees = sum(n),
              percent = (group_trees/total_trees)*100) %>% 
    mutate(type = case_when(str_detect(InfrastructureID, 'RV') == T ~ 'Ruelles vertes',
                            str_detect(InfrastructureID, 'SS') == T ~ 'Segments des rues',
                            str_detect(InfrastructureID, 'CON') == T ~ 'Ruelles grises'),
           city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                            str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières')) %>% 
    filter(flowering != 'not showy') 
  
  
  full_df <- trees_clean %>% 
    group_by(InfrastructureID, city) %>%
    distinct(type) %>% 
    left_join(df, by = "InfrastructureID") %>% 
    mutate(percent = ifelse(is.na(percent), 0, percent))
  
  
   plot <- ggplot(full_df, aes(x = type.x, y = percent, colour = city.x)) + 
    geom_boxplot() + 
    geom_point(position=position_jitterdodge()) + 
    scale_colour_manual(values = c("#6e948c", "#122c43")) +
    theme_classic() + 
    labs(colour = "", x = "", y = "Pourcentage d'arbres à fleurs visibles (%)") + 
    theme(legend.position = "top",
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  
  
  ggsave('graphics/showyflowers.png', plot, height = 10, width = 12, units = 'in')
  
}