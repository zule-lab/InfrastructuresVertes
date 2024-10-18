plot_veg_complexity <- function(ruelle_complexity_raw, street_complexity_raw) {
  
  vegtotal <- ruelle_complexity_raw %>% 
    select(-c(Avarage.of.layers, X)) %>% 
    group_by(InfrastructureID) %>% 
    summarize(total_layers = sum(Layers),
              npoints = n(), 
              avg_complexity = total_layers/npoints) %>% 
    mutate(type = case_when(str_detect(InfrastructureID, 'RV') == T ~ 'Ruelles vertes',
                            str_detect(InfrastructureID, 'CON') == T ~ 'Ruelles grises'),
           city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                            str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières'))
  
  streettotal <- street_complexity_raw %>% 
    select(-c(Comments, X)) %>% 
    group_by(InfrastructureID) %>%
    summarize(total_layers = sum(Layers),
              npoints = n(), 
              avg_complexity = total_layers/npoints) %>% 
    mutate(type = case_when(str_detect(InfrastructureID, 'SS') == T ~ 'Segments des rues'),
           city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                            str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières'))
  
  total <- rbind(vegtotal, streettotal)
  
  plot <- ggplot(total, aes(x = type, y = avg_complexity, colour = city)) + 
    geom_boxplot() + 
    geom_point(position=position_jitterdodge()) + 
    scale_colour_manual(values = c("#6e948c", "#122c43")) +
    theme_classic() + 
    labs(colour = "", x = "", y = "Complexité végétative moyenne (nombre de strates)") + 
    theme(legend.position = "top",
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  
  ggsave('graphics/vegcomplexity.png', plot, width = 8, height = 8, units = 'in')
  
  return(plot)
  
  }
