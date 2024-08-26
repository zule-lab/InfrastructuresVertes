create_study_fig <- function(study_rv, study_controls, quartiers){
  
  rds <- st_read(file.path("/vsizip", 'input/roads.zip')) %>% 
    filter(CATEGORIEC == 'Autoroute')
  
  quartiers_tr <- read_sf('input/Quartiers_3R/Quartier_3R.shp')
  
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
    mutate(group = "Ruelle Verte")
  mont_con <- study_controls %>% 
    filter(CODE_ARR == "VSMPE") %>% 
    mutate(group = "Ruelle Traditionelle")
  
  mont <- rbind(mont_rv, mont_con)
  
  
  insects$insects <- 'oui'
  
  insects <- left_join(mont, st_set_geometry(insects, NULL))  %>% 
    mutate(insects = replace(insects, is.na(insects), 'non'))
  
  insects_pts <- insects %>% 
    st_centroid()
  
  quartiers <- quartiers %>% 
    mutate(Q_socio = replace(Q_socio, Q_socio == 'Parc-Extension', 'Parc-Ex'))
  
  tr_rv <- study_rv %>% 
    filter(CODE_ARR == "TR") %>% 
    mutate(group = "Rruelle Verte")
  tr_con <- study_controls %>% 
    filter(CODE_ARR == "TR") %>%
    mutate(group = "Ruelle Traditionelle")
  tr <- rbind(tr_rv, tr_con)
  
  tr_pts <- st_centroid(tr)
  
  
  
  # Montreal map ------------------------------------------------------------
  
  bb <- st_bbox(st_buffer(mont, 500))
  
  quartiers$nudge_x <- 0
  quartiers$nudge_x[quartiers$Q_socio == "Parc-Ex"] <- -500
  
  quartiers$nudge_y <- 0
  quartiers$nudge_y[quartiers$Q_socio == "Villeray"] <- -250
  
  main <- ggplot() +
    geom_sf(data = insects_pts, aes(size = per_can, colour = insects, fill = group), shape =21, stroke  = 1) +
    geom_sf(data = quartiers, fill = NA, colour = "black", linetype = 'dashed', linewidth = 0.5) +
    geom_sf(data = rds, colour = "black", fill = "black") + 
    scale_colour_manual(values = c(NA, "goldenrod3")) +
    scale_fill_manual(values = c("darkgrey", "darkgreen")) + 
    geom_text(data = quartiers, aes(label = Q_socio, geometry = geometry), nudge_x = quartiers$nudge_x, nudge_y = quartiers$nudge_y, stat = "sf_coordinates") + 
    coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)]) +
    labs(fill = "", colour = "Lucioles", size = "Pourcentage de \ncanopée") + 
    theme(panel.border = element_rect(linewidth = 1, fill = NA),
          panel.background = element_rect(fill = '#f3e3bf'),
          panel.grid = element_blank(),
          axis.text = element_text(size = 16, color = 'black'),
          axis.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          plot.background = element_rect(fill = NA, colour = NA),
          legend.position = 'top')
  
  
  # 3R map ------------------------------------------------------------------
  
  bbtr <- st_bbox(st_buffer(tr, 1000))
  
  bbopq <- st_bbox(st_buffer(st_transform(tr, 4326), 2000))
  
  # roads
  trrds <- opq(bbopq, timeout = 100) %>% 
    add_osm_feature(key = 'route', value = 'road') %>% 
    osmdata_sf()
  bigrds <- trrds$osm_lines
  bigrds <- bigrds %>% filter(name == "Autoroute Félix-Leclerc" | name == "Pont Radisson")
  
  # water
  water <- opq(bbopq, timeout = 100) %>%
    add_osm_feature(key = 'natural', value = 'water') %>%
    osmdata_sf()
  mpols <- water$osm_multipolygons
  
  quartiers_tr$nudge_y <- 0
  quartiers_tr$nudge_y[quartiers_tr$Name == "Immaculé"] <- 500
  
  
  trmap <- ggplot() + 
    geom_sf(data = tr_pts, aes(size = per_can, colour = group)) +
    geom_sf(data = quartiers_tr, fill = NA, colour = "black", linetype = 'dashed', linewidth = 0.5) +
    geom_sf(data = mpols, fill = 'lightblue', colour = "lightblue", linewidth = 0.5) +
    geom_sf(data = bigrds, colour = "black", fill = "black", linewidth = 0.8) + 
    scale_colour_manual(values = c("darkgrey", "darkgreen")) +
    scale_fill_manual(values = c("darkgrey", "darkgreen")) + 
    geom_text(data = quartiers_tr, aes(label = Name, geometry = geometry), nudge_y = quartiers_tr$nudge_y, stat = "sf_coordinates") + 
    coord_sf(xlim = bbtr[c(1, 3)], ylim = bbtr[c(2, 4)]) +
    guides(fill = "none", colour = "none", size = "none") +
    theme(panel.border = element_rect(linewidth = 1, fill = NA),
          panel.background = element_rect(fill = '#f3e3bf'),
          panel.grid = element_blank(),
          axis.text = element_text(size = 16, color = 'black'),
          axis.title = element_blank(),
          plot.background = element_rect(fill = NA, colour = NA))
  
  
  legend <- get_legend(main)
  
  full <- plot_grid(legend, 
                    plot_grid(main + theme(legend.position = 'none'),
                              trmap, align = 'h', labels = c('a) Montréal', 'b) Trois-Rivières'), label_size = 18, vjust = 0.2), 
                    nrow = 2, rel_heights = c(1,5))
  
  ggsave('graphics/studymap.png', full, width = 14, height = 13, units = 'in', dpi = 450)
  
  return(full)
}
