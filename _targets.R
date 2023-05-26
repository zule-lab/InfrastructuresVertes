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
    canopy_path,
    read_canopy('input/31hl102.tif', 'input/31ig102.tif'),
    format = 'file'
    #TODO: add download_file function here
  ),
  
  tar_target(
    rv,
    read_ruelles('input/ruelles-vertes/ruelles-vertes.shp',
                 'input/REQ_ruelles-vertes.kml',
                 'input/TR_ruelles-vertes.kml',
                 st_crs(read_stars(canopy_path)))
    #TODO: add download_file function here
  ),
  
  tar_group_by(
    rv_by_ruelle,
    rv,
    RUELLE_ID
  ),
  
  tar_target(
    can_cov_rv,
    calc_can(rv_by_ruelle, canopy_path),
    map(rv_by_ruelle),
    iteration = 'list'
  ),
  
  tar_target(
    can_cov_rv_bind,
    do.call(rbind, can_cov_rv)
  ),
  
  # Bonus
  tar_group_by(
    rv_by_arr,
    can_cov_rv_bind,
    CODE_ARR
  ),
  
  tar_target(
    plot_rv_by_arr,
    ggplot(rv_by_arr) + geom_sf(aes(fill = canopy)) + theme_bw(),
    map(rv_by_arr),
    iteration = 'list'
  )
  
#  tar_target(
#    survey_rv,
#    st_read('input/VSMPE_surveys_ruelles-vertes.kml') %>%
#      select(-Description) %>%
#      st_as_sf()
#  ),
  
  
)

