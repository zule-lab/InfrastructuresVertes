create_figure_1 <- function(study_rv, study_controls, insects, quartiers){
  
  rds <- shp <- st_read(file.path("/vsizip", 'input/roads.zip')) %>% 
    filter(CATEGORIEC == 'Autoroute')
  
# Data separation ---------------------------------------------------------
  mont_rv <- study_rv %>% 
    filter(CODE_ARR == "VSMPE") %>% 
    mutate(group = "Green Alley")
  mont_con <- study_controls %>% 
    filter(CODE_ARR == "VSMPE") %>% 
    mutate(group = "Control")
  
  mont <- rbind(mont_rv, mont_con)
  
  
  insects$insects <- 'yes'

  insects <- left_join(mont, st_set_geometry(insects, NULL)) 
  
  insects_pts <- insects %>% 
    replace(is.na(insects), 'no') %>% 
    st_centroid()
    
  tr_rv <- study_rv %>% 
    filter(CODE_ARR == "TR")
  tr_con <- study_controls %>% 
    filter(CODE_ARR == "TR")
  tr <- rbind(tr_rv, tr_con)
  
# Montreal context --------------------------------------------------------


# 3R context  -------------------------------------------------------------

  
  

# Montreal map ------------------------------------------------------------
  
  bb <- st_bbox(st_buffer(mont, 500))
  
  # canopy cover?
  # insects 
  
  ggplot() + 
    geom_sf(data = insects_pts, aes(size = per_can), colour = "darkgrey", alpha = 0.75) +
    geom_sf(data = mont, aes(fill = group, colour = group), linewidth = 0.8) +  
    geom_sf(data = quartiers, fill = NA, colour = "black", linetype = 'dashed', linewidth = 0.5) +
    geom_sf(data = rds, colour = "black", fill = "black") + 
    scale_colour_manual(values = c("white", "darkgreen")) +
    scale_fill_manual(values = c("white", "darkgreen")) + 
    geom_text_repel(data = quartiers, aes(label = Q_socio, geometry = geometry), stat = "sf_coordinates") + 
    coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)]) +
    #scale_colour_manual(values = c("white", "darkgreen")) + 
    labs(fill = "", colour = "", size = "Percent Canopy") + 
    theme(panel.border = element_rect(linewidth = 1, fill = NA),
          panel.background = element_rect(fill = '#ddc48d'),
          panel.grid = element_blank(),
          axis.text = element_text(size = 11, color = 'black'),
          axis.title = element_blank(),
          plot.background = element_rect(fill = NA, colour = NA),
          legend.position = 'top')
  

# 3R map ------------------------------------------------------------------



  
}