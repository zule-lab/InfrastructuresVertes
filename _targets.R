# === Targets -------------------------------------------------------------
# Framework by Alec L. Robitaille



# Source ------------------------------------------------------------------
library(targets)
tar_source('R')



# Options -----------------------------------------------------------------
# Targets
tar_option_set(format = 'qs')
options(timeout=100)




# Renv --------------------------------------------------------------------
activate()
snapshot()
restore()



# Targets -----------------------------------------------------------------
data_target <- c(
  
  tar_target(
    canopy,
    read_stars('input/66023_IndiceCanopee_2021.tif')
    #TODO: add download_file function here
  ),
  
  tar_target(
    rv,
    read_sf('input/ruelles-vertes/ruelles-vertes.shp')
    #TODO: add download_file function here
  ),
  
  tar_target(
    can_cov,
    calc_can(rv, canopy)
  )
  
)

