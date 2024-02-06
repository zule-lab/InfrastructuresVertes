create_figure_1 <- function(study_rv, study_controls, insects, quartiers){
  
  rds <- st_read(file.path("/vsizip", 'input/roads.zip')) %>% 
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

  insects <- left_join(mont, st_set_geometry(insects, NULL))  %>% 
    mutate(insects = replace(insects, is.na(insects), 'no'))
  
  insects_pts <- insects %>% 
    st_centroid()
  
  quartiers <- quartiers %>% 
    mutate(Q_socio = replace(Q_socio, Q_socio == 'Parc-Extension', 'Parc-Ex'))
    
  tr_rv <- study_rv %>% 
    filter(CODE_ARR == "TR") %>% 
    mutate(group = "Green Alley")
  tr_con <- study_controls %>% 
    filter(CODE_ARR == "TR") %>%
    mutate(group = "Control")
  tr <- rbind(tr_rv, tr_con)
  
  tr_pts <- st_centroid(tr)
  
  

# Montreal map ------------------------------------------------------------
  
  bb <- st_bbox(st_buffer(mont, 500))
  
  main <- ggplot() +
    geom_sf(data = insects_pts, aes(size = per_can, colour = insects, fill = group), shape =21, stroke  = 1) +
    geom_sf(data = quartiers, fill = NA, colour = "black", linetype = 'dashed', linewidth = 0.5) +
    geom_sf(data = rds, colour = "black", fill = "black") + 
    scale_colour_manual(values = c(NA, "goldenrod3")) +
    scale_fill_manual(values = c("darkgrey", "darkgreen")) + 
    geom_text_repel(data = quartiers, aes(label = Q_socio, geometry = geometry), stat = "sf_coordinates") + 
    coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)]) +
    labs(fill = "", colour = "Fireflies", size = "Percent Canopy") + 
    theme(panel.border = element_rect(linewidth = 1, fill = NA),
          panel.background = element_rect(fill = '#f3e3bf'),
          panel.grid = element_blank(),
          axis.text = element_text(size = 11, color = 'black'),
          axis.title = element_blank(),
          plot.background = element_rect(fill = NA, colour = NA),
          legend.position = 'top')
    

# 3R map ------------------------------------------------------------------
  
  bbtr <- st_bbox(st_buffer(tr, 1000))
  
  bbopq <- st_bbox(st_buffer(st_transform(tr, 4326), 500))
  
  
  trrds <- opq(bbopq) %>% 
    add_osm_feature(key = 'route', value = 'road') %>% 
    osmdata_sf()
  bigrds <- trrds$osm_lines
  bigrds <- bigrds %>% 
    filter(grepl('Autoroute', name) | grepl('Pont Radisson', name)) %>% 
    st_as_sf(st_make_valid())
  
  
  water <- opq(bbopq) %>%
    add_osm_feature(key = 'natural', value = 'water') %>%
    osmdata_sf()
  # We only want multipolygons (aka large rivers)
  mpols <- water$osm_multipolygons
  mpols <- st_cast(mpols, "MULTIPOLYGON")
  mpols <- st_as_sf(st_make_valid(mpols))
  
  trmap <- ggplot() + 
    geom_sf(data = tr_pts, aes(size = per_can, colour = group)) +
    geom_sf(data = mpols, fill = 'lightblue', colour = "lightblue", linewidth = 0.5) +
    geom_sf(data = bigrds, colour = "black", fill = "black", linewidth = 0.8) + 
    scale_colour_manual(values = c("darkgrey", "darkgreen")) +
    scale_fill_manual(values = c("darkgrey", "darkgreen")) +
    coord_sf(xlim = bbtr[c(1, 3)], ylim = bbtr[c(2, 4)]) +
    guides(fill = "none", colour = "none", size = "none") +
    theme(panel.border = element_rect(linewidth = 1, fill = NA),
          panel.background = element_rect(fill = '#f3e3bf'),
          panel.grid = element_blank(),
          axis.text = element_text(size = 11, color = 'black'),
          axis.title = element_blank(),
          plot.background = element_rect(fill = NA, colour = NA))
  
  legend <- get_legend(main)
  plot_grid(legend, 
            plot_grid(main + theme(legend.position = 'none'),
            trmap, align = 'h'), nrow = 2, rel_heights = c(1,5))
  
}