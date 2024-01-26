create_figure_1 <- function(study_rv, study_controls, insects, quartiers){
  

# Data separation ---------------------------------------------------------
  vsmpe <- quartiers %>% 
    filter(Arrondisse == "Villeray–Saint-Michel–Parc-Extension")
  
  
  mont_rv <- study_rv %>% 
    filter(CODE_ARR == "VSMPE") %>% 
    mutate(group = "Green Alley")
  mont_con <- study_controls %>% 
    filter(CODE_ARR == "VSMPE") %>% 
    mutate(group = "Control")
  
  mont <- rbind(mont_rv, mont_con)
  
  insects$insects <- 'yes'

  mont$insects <- insects$insects[match(mont$RUELLE_CODE, insects$RUELLE_CODE)]
  
  
  tr_rv <- study_rv %>% 
    filter(CODE_ARR == "TR")
  tr_con <- study_controls %>% 
    filter(CODE_ARR == "TR")
  tr <- rbind(tr_rv, tr_con)
  
# Montreal context --------------------------------------------------------


# 3R context  -------------------------------------------------------------

  
  

# Montreal map ------------------------------------------------------------
  
  # canopy cover?
  # insects 
  
  ggplot() + 
    geom_sf(data = vsmpe, fill = NA, colour = "black", linewidth = 0.5) + 
    geom_sf(data = mont, aes(fill = group, colour = group)) + 
    scale_fill_manual(values = c("white", "darkgreen")) + 
    scale_colour_manual(values = c("white", "darkgreen")) + 
    labs(fill = "", colour = "") + 
    theme(panel.border = element_rect(linewidth = 1, fill = NA),
          panel.background = element_rect(fill = '#ddc48d'),
          panel.grid = element_line(color = '#73776F', linewidth = 0.2),
          axis.text = element_text(size = 11, color = 'black'),
          axis.title = element_blank(),
          plot.background = element_rect(fill = NA, colour = NA),
          legend.position = 'top')
  

# 3R map ------------------------------------------------------------------



  
}