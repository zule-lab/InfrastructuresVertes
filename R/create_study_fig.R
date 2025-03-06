create_study_fig <- function(study_rv, study_controls, quartiers){
  
  # Insect ruelles ----------------------------------------------------------
  
  all <- rbind(study_rv, study_controls)
  
  insects <- all %>%
    filter(RUELLE_CODE == "RV-VSMPE-2" | 
             RUELLE_CODE == "RV-VSMPE-3" | 
             RUELLE_CODE == "RV-VSMPE-6" | 
             RUELLE_CODE == "RV-VSMPE-8" | 
             RUELLE_CODE == "RV-VSMPE-10" | 
             RUELLE_CODE == "RV-VSMPE-30" | 
             RUELLE_CODE == "RV-VSMPE-22" | 
             RUELLE_CODE == "RV-VSMPE-39" | 
             RUELLE_CODE == "RV-VSMPE-29" |
             RUELLE_CODE == "RV-VSMPE-35" |
             RUELLE_CODE == "RV-VSMPE-26" |
             RUELLE_CODE == "RV-VSMPE-31" |
             RUELLE_CODE == "RV-VSMPE-36" |
             RUELLE_CODE == "RV-VSMPE-25" |
             RUELLE_CODE == "RV-VSMPE-23" |
             RUELLE_CODE == "RV-VSMPE-32" |
             RUELLE_CODE == "RV-VSMPE-40" |
             RUELLE_CODE == "RV-VSMPE-13" |
             RUELLE_CODE == "RV-VSMPE-16" |
             RUELLE_CODE == "RV-VSMPE-17" |
             RUELLE_CODE == "RV-VSMPE-20" |
             RUELLE_CODE == "RV-VSMPE-21" |
             RUELLE_CODE == "CON-VSMPE-1" |
             RUELLE_CODE == "CON-VSMPE-7" |
             RUELLE_CODE == "CON-VSMPE-8" |
             RUELLE_CODE == "CON-VSMPE-10" |
             RUELLE_CODE == "CON-VSMPE-4" )
  
  # Data separation ---------------------------------------------------------
  mont_rv <- study_rv %>% 
    filter(CODE_ARR == "VSMPE") %>% 
    mutate(group = "Green alley")
  mont_con <- study_controls %>% 
    filter(CODE_ARR == "VSMPE") %>% 
    mutate(group = "Grey alley")
  
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
    mutate(group = "Ruelle Verte")
  tr_con <- study_controls %>% 
    filter(CODE_ARR == "TR") %>%
    mutate(group = "Ruelle Traditionelle")
  tr <- rbind(tr_rv, tr_con)
  
  tr_pts <- st_centroid(tr)
  
  
  
  # Montreal map ------------------------------------------------------------
  
  bb <- st_bbox(st_buffer(mont, 500))
  
  bbopq_mtl <- st_bbox(st_buffer(st_make_valid(st_transform(mont, 4326)), 2000))
  
  # roads
  mtlrds <- opq(bbopq_mtl, timeout = 100) %>% 
    add_osm_feature(key = 'highway', value = c('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'residential')) %>% 
    osmdata_sf()
  bigrds_mtl <- mtlrds$osm_lines
  
  
  quartiers$nudge_x <- 0
  quartiers$nudge_x[quartiers$Q_socio == "Parc-Ex"] <- -500
  
  quartiers$nudge_y <- 0
  quartiers$nudge_y[quartiers$Q_socio == "Villeray"] <- -250
  
  main <- ggplot() +
    geom_sf(data = insects_pts, aes(size = per_can, colour = insects, fill = group), shape =21, stroke  = 1, alpha = 0.5) +
    geom_sf(data = bigrds_mtl, colour = "darkgrey", fill = "darkgrey", linewidth = 0.5, alpha = 0.4) +
    geom_sf(data = quartiers, fill = NA, colour = "black", linetype = 'dashed', linewidth = 0.5) +
    geom_sf(data = insects_pts, aes(size = per_can, colour = insects, fill = group), shape =21, stroke  = 1, alpha = 0.5) +
    scale_colour_manual(values = c(NA, "goldenrod3")) +
    scale_fill_manual(values = c("darkgreen", "grey30")) + 
    geom_text(data = quartiers, aes(label = Q_socio, geometry = geometry), nudge_x = quartiers$nudge_x, nudge_y = quartiers$nudge_y, stat = "sf_coordinates") + 
    coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)]) +
    labs(fill = "", colour = "Fireflies", size = "Percent Canopy") + 
    theme(panel.border = element_rect(linewidth = 1, fill = NA),
          panel.background = element_rect(fill = '#f3e3bf'),
          panel.grid = element_blank(),
          axis.text = element_text(size = 16, color = 'black'),
          axis.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.background = element_rect(fill = NA, colour = NA), 
          plot.background = element_rect(fill = NA, colour = NA),
          legend.position = 'right')
  
  
  # 3R map ------------------------------------------------------------------
  
  bbtr <- st_bbox(st_buffer(tr, 1000))
  
  bbopq <- st_bbox(st_buffer(st_transform(tr, 4326), 2000))
  
  # roads
  trrds <- opq(bbopq, timeout = 100) %>% 
    add_osm_feature(key = 'highway', value = c('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'residential')) %>% 
    osmdata_sf()
  bigrds <- trrds$osm_lines
  #bigrds <- bigrds %>% filter(name == "Autoroute Félix-Leclerc" | name == "Pont Radisson")
  
  # water
  water <- opq(bbopq, timeout = 100) %>%
    add_osm_feature(key = 'natural', value = 'water') %>%
    osmdata_sf()
  mpols <- water$osm_multipolygons
  
  #quartiers_tr$nudge_y <- 0
  #quartiers_tr$nudge_y[quartiers_tr$Name == "Immaculé"] <- 500
  
  
  trmap <- ggplot() + 
    geom_sf(data = tr_pts, aes(size = per_can, colour = group), alpha = 0.5) +
    geom_sf(data = mpols, fill = 'lightblue', colour = "lightblue", linewidth = 0.5) +
    geom_sf(data = bigrds, colour = "darkgrey", fill = "darkgrey", linewidth = 0.5, alpha = 0.5) +
    geom_sf(data = tr_pts, aes(size = per_can, colour = group), alpha = 0.5) +
    #geom_sf(data = quartiers_tr, fill = NA, colour = "black", linetype = 'dashed', linewidth = 0.5) +
    scale_colour_manual(values = c("grey30", "darkgreen")) +
    scale_fill_manual(values = c("grey30", "darkgreen")) + 
    #geom_text(data = quartiers_tr, aes(label = Name, geometry = geometry), nudge_y = quartiers_tr$nudge_y, stat = "sf_coordinates") + 
    coord_sf(xlim = bbtr[c(1, 3)], ylim = bbtr[c(2, 4)]) +
    guides(fill = "none", colour = "none", size = "none") +
    theme(panel.border = element_rect(linewidth = 1, fill = NA),
          panel.background = element_rect(fill = '#f3e3bf'),
          panel.grid = element_blank(),
          axis.text = element_text(size = 16, color = 'black'),
          axis.title = element_blank(),
          plot.background = element_rect(fill = NA, colour = NA))
  
  
  legend <- get_plot_component(main, pattern = "guide-box-right")
  
  full <- plot_grid(plot_grid(main + theme(legend.position = 'none'),
                              trmap, align = 'h', labels = c('a) Montréal', 'b) Trois-Rivières'), label_size = 18), 
                    legend, 
                    nrow = 1, rel_widths = c(4,1))
  
  ggsave('graphics/studymap.png', full, width = 18, height = 12, units = 'in', dpi = 450)
  
  return(full)
}
