calc_can <- function(rv, canopy_path){
  
  # ruelle area
  rv_a <- rv %>%
    summarize(RUELLE_ID = RUELLE_ID, 
              CODE_ARR = CODE_ARR, 
              geometry = st_union(geometry)) %>%
    mutate(ruelle_area = st_area(geometry))
  
  
  # canopy area 
  canopy <- read_stars(canopy_path)
  
  # [[1]] to grab the value returned (instead of sf object) to save in 
  #  as column
  rv_a$canopy <- aggregate(
    canopy,
    rv_a, 
    FUN = function(x) sum(!is.na(x))
  )[[1]]
  
  
  # percent canopy cover
  rv_a$per_can <- drop_units(rv_a$canopy/rv_a$ruelle_area)
  
  return(rv_a)
  
}