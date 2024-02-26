plot_pa_firefly <- function(fireflies_raw){

    
  
  presencecnt <- fireflies_raw %>% 
    group_by(Infrastructure.ID, Presence.Absence) %>% 
    tally() %>% 
    mutate(type = case_when(str_detect(Infrastructure.ID, 'RV') == T ~ 'Ruelles Vertes',
                            str_detect(Infrastructure.ID, 'SS') == T ~ 'Segements des Rues',
                            str_detect(Infrastructure.ID, 'CON') == T ~ 'Ruelles Traditionelles')) %>%
    filter(Presence.Absence != "Absence") %>% 
    group_by(type) %>% 
    tally()
  
  plot <- ggplot(presencecnt) + 
    geom_col(aes(x = type, y = n)) + 
    theme_classic() + 
    labs(x = "", y = "Nombre de sites avec présence de lucioles", title = "5 ruelles traditionelles au total, 22 ruelles vertes au total, 27 segments des rues au total")
  
  ggsave('graphics/fireflypresence.png', plot, width = 8, height = 8, units = 'in')

  return(plot)  
}