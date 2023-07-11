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
                 'input/TR_ruelles.kml',
                 st_crs(read_stars(canopy_path)))
    #TODO: add download_file function here
  ),
  
  tar_target(
    controls,
    read_controls('input/voi_voirie_s_v22_shp/VOI_VOIRIE_S_V22.shp',
                  'input/TR_ruelles.kml',
                  canopy_path)
    #TODO: add download_file function here
  ),
   
  tar_target(
    survey_rv,
    read_sf('input/VSMPE_surveys_ruelles-vertes.kml') %>%
      select(-description) %>%
      mutate(survey = "TRUE")
    #TODO: add download_file function here
  ),
  
  tar_target(
    quartiers,
    read_sf('input/quartiers/quartiers_sociologiques_2014.shp')
    #TODO: add download_file function here
  ),

  tar_group_by(
    rv_by_ruelle,
    rv,
    RUELLE_ID
  ),
  
  tar_group_by(
    controls_by_ruelle,
    controls,
    RUELLE_ID
  ),
  
  tar_target(
    can_cov_rv,
    calc_can(rv_by_ruelle, canopy_path),
    map(rv_by_ruelle),
    iteration = 'list'
  ),
  
  tar_target(
    can_cov_controls,
    calc_can(controls_by_ruelle, canopy_path),
    map(controls_by_ruelle),
    iteration = 'list'
  ),
  
  tar_target(
    can_cov_rv_bind,
    do.call(rbind, can_cov_rv)
  ),
  
  tar_target(
    can_cov_controls_bind,
    do.call(rbind, can_cov_controls)
  ),
  
  tar_target(
    study_rv,
    select_study(can_cov_rv_bind, quartiers)
  ),
  
  tar_target(
    study_controls,
    select_controls(can_cov_controls_bind, quartiers)
  ),
  
  tar_target(
    insects,
    select_insects(study_rv, study_controls)
  )
  
  
)

