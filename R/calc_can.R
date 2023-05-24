calc_can <- function(rv, canopy){
  
  # match projections
  rv_t <- rv %>% st_transform(st_crs(canopy)) %>%
    # create 10 m buffer around ruelles
    st_buffer(dist = 10) %>%
    # select Villeray-Saint Michel-Parc Extension
    filter(CODE_ARR == "VSMPE") 
  
  # calculate canopy cover by dividing number of pixels == 4 (canopy) by total number of pixels within a buffer
  rv_can <- aggregate(canopy, rv_t, FUN = function(x) sum(x == 4)/length(x)) %>% 
    st_as_sf() %>%
    rename(percan_2021 = `66023_IndiceCanopee_2021.tif`) %>%
    mutate(percan_2021 = round(percan_2021, 3),
           id = rv_t$RUELLE_ID)
    
  return(rv_can)
  
}