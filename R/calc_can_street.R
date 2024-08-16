calc_can_street <- function(street_segments_rv, street_segments_control, sidewalks, tr_roads, canopy_path){
  
  # VSMPE 
  streets_vsmpe_rv <- clean_ss(street_segments_rv, sidewalks, 'VSMPE', 3, canopy_path)
  
  streets_vsmpe_controls <- clean_ss(street_segments_control, sidewalks, 'VSMPE', 3, canopy_path)

  # TR
  streets_tr_rv <- clean_ss(street_segments_rv, tr_roads, 'TR', 7, canopy_path)
  
  streets_tr_controls <- clean_ss(street_segments_control, tr_roads, 'TR', 7, canopy_path)
  
  # bind
  streets_can <- rbind(streets_vsmpe_rv, streets_vsmpe_controls, streets_tr_rv, streets_tr_controls)
  
  return(streets_can)
  
}

clean_ss <- function(street_segments, roads, quartier, buffer_dist, canopy_path){
  
  segments_t <- st_transform(street_segments, st_crs(roads)) %>% 
    select(c(Name, geometry))
  
  streets <- st_join(segments_t %>% filter(str_detect(Name, quartier)), roads, join = st_nearest_feature) %>% 
    st_drop_geometry() %>% 
    left_join(., roads) %>% 
    st_as_sf() %>% 
    st_buffer(dist = buffer_dist, endCapStyle = "SQUARE")
  
  # read in canopy
  canopy <- read_stars(canopy_path)
  streets_can <- st_transform(streets, st_crs(canopy)) %>% 
    mutate(street_area = st_area(geometry))
  
  # [[1]] to grab the value returned (instead of sf object) to save in 
  #  as column
  streets_can$canopy <- aggregate(
    canopy,
    streets_can, 
    FUN = function(x) sum(!is.na(x))
  )[[1]]
  
  
  # percent canopy cover
  streets_can$per_can <- drop_units(streets_can$canopy/streets_can$street_area)
  
  streets_f <- streets_can %>% 
    select(c(Name, street_area, canopy, per_can, geometry))
  
  return(streets_f)
  
}
