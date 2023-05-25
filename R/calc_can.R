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
  
  # calculate canopy cover by dividing number of pixels == 4 (canopy) by total number of pixels within a buffer
  rv_can <- aggregate(canopy, rv_b, FUN = function(x) sum(x == 4)/length(x)) %>% 
    st_as_sf() %>%
    rename(percan_2021 = `66023_IndiceCanopee_2021.tif`) %>%
    mutate(percan_2021 = round(percan_2021, 3),
           id = rv_t$RUELLE_ID)
    
  return(rv_can)
  
}