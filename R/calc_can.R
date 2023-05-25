calc_can <- function(rv, canopy){
  
  # match projections
  rv_t <- rv %>% st_transform(st_crs(canopy)) %>%
    # filter for the neighbourhood
    filter(CODE_ARR == "VSMPE") %>%
    # select relevant columns
    select(RUELLE_ID, CODE_ARR, geometry) %>%
    # combine ruelles with the same ID
    group_by(RUELLE_ID) %>%
    summarize(geometry = st_union(geometry)) %>%
    mutate(ruelle_area = st_area(geometry))
  
  # make a buffer of 10 m to make sure edges of ruelles are included 
  rv_b <- st_buffer(rv_t, 10)
  
  # intersect canopy with ruelles
  can_i <- canopy[rv_b, ]
  
  # convert into sf object
  can_sf <- st_as_sf(can_i)
  
  # associate with ruelles
  can_a <- st_intersection(can_sf, rv_b)
  
  # union pixels for each ruelle, calculate their area, and then % canopy
  can_cov <- can_a %>% 
    group_by(RUELLE_ID) %>% 
    summarize(geometry = st_union(geometry), 
              ruelle_area = first(ruelle_area), 
              canopy_area = st_area(geometry),
              per_can = canopy_area/ruelle_area) %>%
    st_drop_geometry()
  
  # associate with ruelle geometry
  can_rv <- inner_join(can_cov, rv_b, by = "RUELLE_ID")
  

  return(can_cov)
  
}