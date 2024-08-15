plot_veg_complexity <- function(ecological_benefits) {
  
  
  plot <- ggplot(ecological_benefits, aes(x = type, y = avg_complexity, colour = city)) + 
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
