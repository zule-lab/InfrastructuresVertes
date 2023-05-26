read_canopy <- function(mtl_can, tr_can) {
  
  mtl <- read_stars(mtl_can)
  
  tr <- read_stars(tr_can)
  
  can <- st_mosaic(mtl, tr)
  
  return(can$canopy_mosaic)
  
}