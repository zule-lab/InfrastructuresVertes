read_canopy <- function(mtl_can, tr_can) {
  

  #mtl <- download_tif('https://cartes.inspq.qc.ca/diffusion/donnee_publique/canopee/31hl102.tif', 'input/mon-canopy.tif')
  #tr <- download_tif('https://cartes.inspq.qc.ca/diffusion/donnee_publique/canopee/31ig102.tif', 'input/tr-canopy.tif')
  
  mtl <- read_stars('input/mon-canopy.tif', proxy = TRUE)
  
  tr <- read_stars('input/tr-canopy.tif', proxy = TRUE)
  
  # Save mosaic as file in output/
  can <- st_mosaic(mtl, tr, dst = file.path('output', 'canopy_mosaic.tif'))
  
  # Return path of output mosaic
  return(can$canopy_mosaic)
  
}