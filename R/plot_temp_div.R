plot_temp_div <- function(temp_plot, tree_species){
  
  # filter tree div dataset for only ruelles vertes 
  rv_div <- tree_species %>% 
    filter(type == "Ruelles Vertes") %>% 
    group_by(city) %>% 
    slice_max(SpeciesRichness, n = 3) %>% 
    ungroup() %>% 
    select(c(InfrastructureID, SpeciesRichness)) 
  
  # join with temperature data 
  max_div <- inner_join(temp_plot, rv_div, by = join_by(plot_id == InfrastructureID))
  
  # labels
  cities <- c(
    TR = "Trois-Rivières",
    VSMPE = "Villeray-Saint Michel-Parc Extension"
  )
  
  time <- c(
    day = "Day",
    night = "Night"
  )
  
  # plot
   tradeoff <- ggplot(data = temp_plot, aes(date, mean_cooling, group = plot_id)) +
    geom_line(colour = "grey20", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = 2, color = "black", linewidth = 1 ) + 
    geom_line(data = max_div, aes(colour = SpeciesRichness), linewidth = 1) +
    scale_colour_continuous(trans = 'reverse') +  
    labs(x = "", colour = "Le nombre \nd'espèces \nd'arbres", y = "Effet de refroidissement (\u00B0C)") + 
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    facet_wrap(vars(CODE_ARR, tod), scales = "free", labeller = labeller(CODE_ARR = as_labeller(cities), tod = as_labeller(time), .multi_line = F))
  
  
  # save
  ggsave('graphics/tempdiv_tradeoff.png', tradeoff, height = 10, width = 14, units = 'in')
  
  return(tradeoff)
  
}



