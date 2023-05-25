calc_can <- function(rv, canopy){
  
  # match projections
  rv_t <- rv %>% st_transform(st_crs(canopy)) %>%
    # filter for the neighbourhood
    filter(CODE_ARR == "VSMPE") %>%
    # select relevant columns
    select(RUELLE_ID, CODE_ARR, geometry) %>%
    # combine ruelles with the same ID
    group_by(RUELLE_ID) %>%
    summarize(geometry = st_union(geometry))
  
  # make a buffer of 10 m to make sure edges of ruelles are included 
  rv_b <- st_buffer(rv_t, 10)

    
  return(rv_can)
  
}