calc_can <- function(rv, canopy){
  
  # match projections
  rv_t <- rv %>% st_transform(st_crs(canopy)) %>%
    # filter for the neighbourhood
    filter(CODE_ARR == "VSMPE") %>%
    # select relevant columns
    select(RUELLE_ID, CODE_ARR, geometry) %>%
    # make 10 m buffer to ensure edges of ruelles are included
    st_buffer(10) %>%
    # combine ruelles with the same ID
    group_by(RUELLE_ID) %>%
    summarize(geometry = st_union(geometry)) %>%
    mutate(ruelle_area = st_area(geometry))
  
  # intersect canopy with ruelles
  can_i <- canopy[rv_t, ]
  
  # convert into sf object
  can_sf <- st_as_sf(can_i)
  
  # associate with ruelles
  can_a <- st_intersection(can_sf, rv_t)
  
  # union pixels for each ruelle, calculate their area, and then % canopy
  can_cov <- can_a %>% 
    group_by(RUELLE_ID) %>% 
    summarize(geometry = st_union(geometry), 
              ruelle_area = first(ruelle_area), 
              canopy_area = st_area(geometry),
              per_can = canopy_area/ruelle_area) %>%
    st_drop_geometry()
  
  # associate with ruelle geometry
  can_rv <- inner_join(can_cov, rv_t, by = "RUELLE_ID") %>%
    rename(ruelle_area = ruelle_area.x) %>%
    select(-ruelle_area.y) %>%
    st_as_sf()
  

  return(can_rv)
  
}