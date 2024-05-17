mit_temp <- function(temp_dfs, tr_temp_dfs, study_rv, study_controls){
  
  # make sure ruelle names are formatted correctly, select relevant columns, rename columns, and remove first line of df (units)
  temp_names <- temp_dfs %>% 
    mutate(plot_id = str_replace(plot_id, "CON", "CON-VSMPE-"),
           plot_id = str_replace(plot_id, "RV", "RV-VSMPE-"))
  
  study <- rbind(study_controls, study_rv)
  
  # join temp data with site info 
  temp_vsmpe_geom <- left_join(temp_names, study, by = join_by(plot_id == RUELLE_CODE))
  temp_tr_geom <- left_join(tr_temp_dfs, study, by = join_by(plot_id == RUELLE_CODE))
  
  # combine all temp data 
  temp_geom <- rbind(temp_vsmpe_geom, temp_tr_geom)
  
  # convert from farenheit to celsius 
  temp_cel <- temp_geom %>% 
    mutate(temp_F = str_replace_all(temp_F, ',', '.'), 
           heat_index_F = str_replace_all(heat_index_F, ',', '.'), 
           dew_point_F = str_replace_all(dew_point_F, ',', '.'), 
           temp_C = round(((as.numeric(temp_F) - 32)*5)/9, 1),
           heat_index_C = round(((as.numeric(heat_index_F) - 32)*5)/9, 1),
           dew_point_C = round(((as.numeric(dew_point_F) - 32)*5)/9, 1))
  
  # format date time / two formats in one dataset - am/pm and 24h
  dt_am <- temp_cel %>% 
    filter(str_detect(date_time, 'a.m.') | str_detect(date_time, 'p.m.')) %>% 
    mutate(date_time = str_replace_all(date_time, '[\\.]',''),
           date_time = strptime(date_time, format = "%Y-%m-%d %I:%M:%S %p"))
  
  dt_24 <- temp_cel %>% 
    filter(!str_detect(date_time, 'a.m.')) %>%
    filter(!str_detect(date_time, 'p.m.')) %>% 
    mutate(date_time = strptime(date_time, format = "%Y-%m-%d %H:%M:%S"))
  
  temp_date <- rbind(dt_am, dt_24)
  
  # select for study period 
  
  # calculate daytime hours
  temp_centroid <- st_as_sf(temp_cel) %>% 
    st_centroid()
  
  temp_coords <- temp_centroid %>% 
    mutate(date = date(date_time)) 
  
  
  # 
  
  
}