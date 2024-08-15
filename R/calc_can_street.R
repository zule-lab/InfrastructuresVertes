calc_can_street <- function(street_segments_rv, street_segments_control, sidewalks){
  
  street_segments_t <- st_transform(street_segments_rv, st_crs(sidewalks)) %>%
    select(c(Name, geometry)) 
  
  control_segments_t <- st_transform(street_segments_control, st_crs(sidewalks)) %>% 
    select(c(Name, geometry))
  
  streets_vsmpe_rv <- st_join(street_segments_t %>% filter(str_detect(Name, 'VSMPE')), sidewalks, join = st_nearest_feature) %>% 
    st_drop_geometry() %>% 
    left_join(., sidewalks) %>% 
    st_as_sf()
  
  streets_vsmpe_controls <- st_join(control_segments_t %>% filter(str_detect(Name, 'VSMPE')), sidewalks, join = st_nearest_feature) %>% 
    st_drop_geometry() %>% 
    left_join(., sidewalks) %>% 
    st_as_sf()
  
  # https://www.donneesquebec.ca/recherche/dataset/reseau-routier-v3r
  # buffer
  # canopy cover calc
  
  
}