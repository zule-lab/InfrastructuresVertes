calc_can <- function(rv, canopy_path){
  
  canopy <- read_stars(canopy_path)
  
  rv$canopy <- aggregate(
    canopy,
    rv, 
    FUN = function(x) sum(!is.na(x))
  )[[1]]
  
  return(rv)
  
  # rv_t <- rv %>%
  #   # combine ruelles with the same ID
  #   group_by(RUELLE_ID) %>%
  #   summarize(geometry = st_union(geometry)) %>%
  #   mutate(ruelle_area = st_area(geometry))
  # 
  # # intersect canopy with ruelles
  # can_i <- canopy[rv_t, ]
  # 
  # can_a <- aggregate(can_i, rv_t, FUN = function(x){sum(!is.na(x))}) %>% 
  #   st_as_sf() %>%
  #   # number of pixels = area because pixels are 1m2
  #   rename(canopy_area = `file4eb83feb10d8.tif`) %>%
  #   mutate(RUELLE_ID = rv_t$RUELLE_ID) %>%
  #   st_drop_geometry()
  # 
  # can_rv <- inner_join(can_a, rv_t, by = "RUELLE_ID") %>%
  #   mutate(per_can = drop_units(canopy_area/ruelle_area)) %>%
  #   st_as_sf()
  # 
  # 
  # return(can_rv)
  
}