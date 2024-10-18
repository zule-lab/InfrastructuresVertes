plot_pa_firefly <- function(fireflies_raw){

    
  
  presencecnt <- fireflies_raw %>% 
    group_by(Infrastructure.ID, Presence.Absence) %>% 
    tally() %>% 
    mutate(type = case_when(str_detect(Infrastructure.ID, 'RV') == T ~ 'Ruelles vertes',
                            str_detect(Infrastructure.ID, 'SS') == T ~ 'Segments des rues',
                            str_detect(Infrastructure.ID, 'CON') == T ~ 'Ruelles grises')) %>%
    filter(Presence.Absence != "Absence") %>% 
    group_by(type) %>% 
    tally() %>% 
    mutate(n = case_when(type == 'Ruelles vertes' ~ (n/22)*100,
                         type == 'Ruelles grises' ~ (n/5)*100,
                         type == 'Segments des rues' ~ (n/27)*100 ))
  
  positions <- c('Ruelles vertes', 'Ruelles grises', 'Segments des rues')
  
  plot <- ggplot(presencecnt) + 
    geom_col(aes(x = type, y = n)) + 
    theme_classic() + 
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12, colour = "black")) +
    scale_x_discrete(limits = positions) + 
    labs(x = "", y = "Pourcentage de sites avec présence de lucioles (%)", title = "Villeray-Saint Michel-Parc Extension")
  
  ggsave('graphics/fireflypresence.png', plot, width = 8, height = 8, units = 'in')

  return(plot)  
}