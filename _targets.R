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
    read_stars('input/31hl102.tif', proxy = F)
    #TODO: add download_file function here
  ),
  
  tar_target(
    rv,
    read_ruelles('input/ruelles-vertes/ruelles-vertes.shp',
                 'input/REQ_ruelles-vertes.kml',
                 'input/TR_ruelles-vertes.kml',
                 canopy)
    #TODO: add download_file function here
  ),
  
#  tar_target(
#    survey_rv,
#    st_read('input/VSMPE_surveys_ruelles-vertes.kml') %>%
#      select(-Description) %>%
#      st_as_sf()
#  ),
  
  tar_target(
    can_cov_rv,
    calc_can(rv, canopy)
  )
  
)

