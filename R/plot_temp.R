plot_temp <- function(temp_mit) {
  
  
  cooling_per_day <- temp_mit %>% 
    mutate(type = case_when(str_detect(plot_id, "CON") == T ~ "Ruelle Traditionelle",
                            str_detect(plot_id, "RV") == T ~ "Ruelle Verte"),
           date = date(date_time)) %>% 
    group_by(plot_id, date, tod) %>% 
    summarize(type = first(type), 
              CODE_ARR = first(CODE_ARR), 
              mean_cooling = mean(cooling, na.rm = T))
  
  
  day_mtl <- plot_groups(cooling_per_day, "VSMPE", "day", "Jour")
  
  night_mtl <- plot_groups(cooling_per_day, "VSMPE", "night", "Nuit")
  
  day_tr <- plot_groups(cooling_per_day, "TR", "day", "Jour")
  
  night_tr <- plot_groups(cooling_per_day, "TR", "night", "Nuit")

  # combine plots
  mtl <- day_mtl | night_mtl
  tr <- day_tr | night_tr
  
  # save
  ggsave('graphics/mtltemp.png', mtl, height = 10, width = 14, units = 'in')
  ggsave('graphics/trtemp.png', tr, height = 10, width = 14, units = 'in')
  
  return(cooling_per_day)
  
}


plot_groups <- function(cooling_per_day, code, timeofday, lab){
  
  df <- cooling_per_day %>% 
    filter(CODE_ARR == code, tod == timeofday & type == "Ruelle Verte")
  
  ggplot(df, aes(date, mean_cooling, group = plot_id)) +
    geom_line(alpha = 0.5, color = "grey20") +
    geom_hline(yintercept = 0, linetype = 2, color = "black", linewidth = 1 ) + 
    labs(x = "", colour = "", y = "Effet de refroidissement (\u00B0C)", title = lab) + 
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "none",
          axis.text = element_text(size = 16, color = 'black'),
          axis.title = element_text(size = 16), 
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          plot.title = element_text(size=16))
  
}
