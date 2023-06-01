read_canopy <- function(mtl_can, tr_can) {
  
  mtl <- read_stars(mtl_can)
  
  tr <- read_stars(tr_can)
  
  # Save mosaic as file in results/
  can <- st_mosaic(mtl, tr, dst = file.path('results', 'canopy_mosaic'))
  
  # Return path of output mosaic
  return(can$canopy_mosaic)
  
}