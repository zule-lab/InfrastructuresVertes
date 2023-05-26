read_canopy <- function(mtl_can, tr_can) {
  
  mtl <- read_stars(mtl_can)
  
  tr <- read_stars(tr_can)
  
  can <- st_mosaic(mtl, tr, dst = file.path('results', 'canopy_mosaic'))
  
  return(can$canopy_mosaic)
  
}