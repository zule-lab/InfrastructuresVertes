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
  
  tar_file_read(
    trees_raw,
    'input/field_data/trees.csv',
    read.csv(!!.x)
  ),
  
  tar_file_read(
    fireflies_raw,
    'input/field_data/firefly-survey.csv',
    read.csv(!!.x)
  ),
  
  tar_file_read(
    ruelle_complexity_raw,
    'input/field_data/ruelle-complexity.csv',
    read.csv(!!.x)
  ),
  
  tar_file_read(
    street_complexity_raw,
    'input/field_data/street-complexity.csv',
    read.csv(!!.x)
  ),
  
  tar_files(
    temp_files,
    dir('input/field_data/temperature-data', full.names = TRUE)
  ),
  
  tar_files(
    temp_files_tr,
    dir('input/field_data/temperature-data-tr/', full.names = TRUE)
  ),
  
  tar_target(
    temp_dfs, 
    # skip problematic lines in dataset including column names
    read_csv(temp_files, skip = 5, col_types = cols(.default = col_character()), col_names = F) %>%  
      # add back in column names
      rename(date_time = X1,
             temp_F = X2,
             rel_humidity_per = X3,
             heat_index_F = X4,
             dew_point_F = X5,
             point_type = X6) %>% 
      # add plot ID column based on file name 
      mutate(plot_id = str_extract(basename(xfun::sans_ext(temp_files)), "[^_]+")) %>%
      # replace commas with decimals for numeric columns
      mutate(across(c("temp_F", "rel_humidity_per", "heat_index_F", "dew_point_F"), ~as.numeric(str_replace(.x, ",", ".")))) %>% 
      # remove unnecessary column 
      select(-point_type),
    pattern = map(temp_files)
  ),
  
  tar_target(
    tr_temp_dfs, 
    # skip problematic lines in dataset including column names
    read_csv(temp_files_tr, skip = 5, col_types = cols(.default = col_character()), col_names = F) %>%  
      # add back in column names
      rename(date_time = X1,
             temp_F = X2,
             rel_humidity_per = X3,
             heat_index_F = X4,
             dew_point_F = X5,
             point_type = X6) %>% 
      # add plot ID column based on file name 
      mutate(plot_id = str_extract(basename(xfun::sans_ext(temp_files_tr)), "[^_]+")) %>%
      # replace commas with decimals for numeric columns
      mutate(across(c("temp_F", "rel_humidity_per", "heat_index_F", "dew_point_F"), ~as.numeric(str_replace(.x, ",", ".")))) %>% 
      # remove unnecessary column 
      select(-point_type),
    pattern = map(temp_files_tr)
  ),
  
  tar_target(
    canopy_path,
    read_canopy(),
    format = 'file'
  ),
  
  tar_target(
    rv,
    read_ruelles('https://donnees.montreal.ca/dataset/ab3ce7bb-09a7-49d7-8f76-461ed4c39937/resource/e1440534-f438-43d3-ab7b-bcd09d72d3cd/download/ruelles-vertes.zip',
                 'input/REQ_ruelles-vertes.kml',
                 'input/TR_ruelles.kml',
                 st_crs(read_stars(canopy_path)))
  ),
  
  tar_target(
    controls,
    read_controls('https://donnees.montreal.ca/dataset/0acbc6c8-bbfc-4aae-a0fa-ec74ba0686c6/resource/102dd6af-836d-443e-9bee-bfdd2f525fb8/download/voi_voirie_s_v22_shp.zip',
                  'input/TR_ruelles.kml',
                  canopy_path)
  ),
   
  tar_target(
    survey_rv,
    read_sf('input/VSMPE_surveys_ruelles-vertes.kml') %>%
      select(-description) %>%
      mutate(survey = "TRUE")
  ),
  
  tar_target(
    quartiers,
    download_shp('https://donnees.montreal.ca/dataset/c8f37ad6-16ff-4cdc-9e5a-e47898656fc9/resource/d342d18e-f710-4991-a259-0092bac3d62c/download/quartiers_sociologiques_2014.zip', 'input/quartiers.zip')
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
  ),
  
  tar_target(
    figure_1,
    create_figure_1(study_rv, study_controls, insects, quartiers)
  ),
  
  tar_target(
    trees_clean,
    clean_trees(trees_raw)
  ),
  
  tar_target(
    tree_species,
    plot_tree_species(trees_clean)
  ),
  
  tar_target(
    tree_abundance,
    plot_tree_abund(trees_clean)
  ),
  
  tar_target(
    firefly_pa, 
    plot_pa_firefly(fireflies_raw)
  ),
  
  tar_target(
    veg_complexity,
    plot_veg_complexity(ruelle_complexity_raw, street_complexity_raw)
  ),

  tar_target(
    temp_mit,
    mit_temp(temp_dfs, tr_temp_dfs, study_rv, study_controls)
  ),
  
  #tar_target(
  #  temp_plot,
  #  plot_temp(temp_dfs, tr_temp_dfs)
  #),
  
  # model temp? or still plot raw? 
  
  # biodiversity of alleys -- trees? 
  
  tar_target(
    tree_div,
    div_tree(trees_clean)
  )
  

  
)

