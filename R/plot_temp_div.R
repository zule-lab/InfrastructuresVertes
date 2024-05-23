plot_temp_div <- function(temp_plot, tree_species){
  
  # filter tree div dataset for only ruelles vertes 
  rv_div <- tree_species %>% 
    filter(type == "Ruelles Vertes") %>% 
    group_by(city, InfrastructureID) %>% 
    slice_max(SpeciesRichness, n = 3) %>%
    ungroup() %>% 
    select(c(InfrastructureID, SpeciesRichness)) %>% 
    mutate(SpeciesRichness = as.factor(SpeciesRichness))
  
  # join with temperature data 
  max_div <- inner_join(temp_plot, rv_div, by = join_by(plot_id == InfrastructureID))
  
  
  
  ggplot(data = temp_plot, aes(date, mean_cooling, group = plot_id)) +
    geom_line(colour = "grey20", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = 2, color = "black", linewidth = 1 ) + 
    geom_line(data = max_div, aes(colour = SpeciesRichness), linewidth = 1) + 
    scale_colour_manual(values = c("#FDBB84", "#FC8D59", "#E34A33", "#B30000")) + 
    labs(x = "", colour = "", y = "Effet de refroidissement (\u00B0C)") + 
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    facet_wrap(vars(CODE_ARR, tod), scales = "free")
  
  
  
  
  
  
  
  
  # plot 
  day_mtl <- plot_groups_w_div(temp_plot, max_div, "VSMPE", "day", "Jour")
  
  night_mtl <- plot_groups_w_div(full, "VSMPE", "night", "Nuit")
  
  day_tr <- plot_groups_w_div(full, "TR", "day", "Jour")
  
  night_tr <- plot_groups_w_div(full, "TR", "night", "Nuit")
  
  # combine plots
  mtl <- day_mtl | night_mtl
  tr <- day_tr | night_tr
  
  # save
  ggsave('graphics/mtltemp.png', mtl, height = 10, width = 14, units = 'in')
  ggsave('graphics/trtemp.png', tr, height = 10, width = 14, units = 'in')
  
  return(cooling_per_day)
  
}


plot_groups_w_div <- function(full, max_div, code, timeofday, lab){
  
  df <- full %>% 
    filter(CODE_ARR == code, tod == timeofday & type == "Ruelle Verte")
  
  df2 <- max_div %>% 
    filter(CODE_ARR == code, tod == timeofday & type == "Ruelle Verte")
  
  ggplot(data = df, aes(date, mean_cooling, group = plot_id)) +
    geom_line(colour = "grey20", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = 2, color = "black", linewidth = 1 ) + 
    geom_line(data = df2, aes(colour = SpeciesRichness), linewidth = 1) + 
    scale_color_brewer(palette = "OrRd") + 
    labs(x = "", colour = "", y = "Effet de refroidissement (\u00B0C)", title = lab) + 
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
}
