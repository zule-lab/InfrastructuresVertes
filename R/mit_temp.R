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
  
  # select for study period Jun 28 17:00 - Jul 25 9:00 for mtl / Aug 8 17:00 - Oct 3 9:00 for tr
  temp_study <- temp_date %>% 
    filter(date_time > "2023-06-28 17:00:00 EDT" & date_time < "2023-10-03 9:00:00 EDT") %>% 
    filter(date_time < "2023-07-25 9:00:00 EDT" | date_time > "2023-08-08 17:00:00 EDT")
  
  # calculate daytime hours
  temp_coords <- st_as_sf(temp_study) %>% 
    st_centroid() %>% 
    st_transform(4326) %>%
    mutate(date = date(date_time),
           lon = st_coordinates(geometry)[,1],
           lat = st_coordinates(geometry)[,2])
  
  # calculate for each entry if it is during the daytime or nighttime based on the tod + sunrise/sunset
  temp_tod <- temp_coords %>% 
    select(c(date, lat, lon)) %>% 
    getSunlightTimes(data = ., tz = 'America/Toronto', keep = c('sunrise', 'sunset')) %>% 
    select(c(sunrise, sunset)) %>% 
    cbind(temp_coords) %>%
    mutate(tod = case_when(date_time >= sunrise & date_time <= sunset ~ 'day',
                           date_time < sunrise | date_time > sunset ~ 'night')) %>% 
    select(-c(sunrise, sunset, date, lat, lon))
  
  # match ruelles with their control sites 
  con <- temp_tod %>% 
    filter(str_detect(plot_id, 'CON')) %>% 
    rename(con_id = plot_id) %>% 
    select(c(date_time, con_id, per_can, Q_socio, temp_C, rel_humidity_per, heat_index_C))

  rv <- temp_tod %>% 
    filter(str_detect(plot_id, 'RV')) %>% 
    mutate(con_id = case_when(plot_id == 'RV-VSMPE-7' ~ 'CON-VSMPE-1',
                              plot_id == 'RV-VSMPE-9' ~ 'CON-VSMPE-1',
                              plot_id == 'RV-VSMPE-3' ~ 'CON-VSMPE-1',
                              plot_id == 'RV-VSMPE-5' ~ 'CON-VSMPE-1',
                              plot_id == 'RV-VSMPE-4' ~ 'CON-VSMPE-2',
                              plot_id == 'RV-VSMPE-6' ~ 'CON-VSMPE-2',
                              plot_id == 'RV-VSMPE-8' ~ 'CON-VSMPE-2',
                              plot_id == 'RV-VSMPE-1' ~ 'CON-VSMPE-3',
                              plot_id == 'RV-VSMPE-2' ~ 'CON-VSMPE-3',
                              plot_id == 'RV-VSMPE-10' ~ 'CON-VSMPE-3',
                              plot_id == 'RV-VSMPE-39' ~ 'CON-VSMPE-7',
                              plot_id == 'RV-VSMPE-33' ~ 'CON-VSMPE-7',
                              plot_id == 'RV-VSMPE-35' ~ 'CON-VSMPE-7',
                              plot_id == 'RV-VSMPE-37' ~ 'CON-VSMPE-7',
                              plot_id == 'RV-VSMPE-27' ~ 'CON-VSMPE-10',
                              plot_id == 'RV-VSMPE-34' ~ 'CON-VSMPE-10',
                              plot_id == 'RV-VSMPE-36' ~ 'CON-VSMPE-10',
                              plot_id == 'RV-VSMPE-23' ~ 'CON-VSMPE-10',
                              plot_id == 'RV-VSMPE-28' ~ 'CON-VSMPE-10',
                              plot_id == 'RV-VSMPE-40' ~ 'CON-VSMPE-9',
                              plot_id == 'RV-VSMPE-22' ~ 'CON-VSMPE-9',
                              plot_id == 'RV-VSMPE-30' ~ 'CON-VSMPE-9',
                              plot_id == 'RV-VSMPE-25' ~ 'CON-VSMPE-9',
                              plot_id == 'RV-VSMPE-26' ~ 'CON-VSMPE-9',
                              plot_id == 'RV-VSMPE-31' ~ 'CON-VSMPE-9',
                              plot_id == 'RV-VSMPE-32' ~ 'CON-VSMPE-8',
                              plot_id == 'RV-VSMPE-24' ~ 'CON-VSMPE-8',
                              plot_id == 'RV-VSMPE-29' ~ 'CON-VSMPE-8',
                              plot_id == 'RV-VSMPE-38' ~ 'CON-VSMPE-8',
                              plot_id == 'RV-VSMPE-11' ~ 'CON-VSMPE-6',
                              plot_id == 'RV-VSMPE-12' ~ 'CON-VSMPE-6',
                              plot_id == 'RV-VSMPE-14' ~ 'CON-VSMPE-6',
                              plot_id == 'RV-VSMPE-15' ~ 'CON-VSMPE-6',
                              plot_id == 'RV-VSMPE-16' ~ 'CON-VSMPE-5',
                              plot_id == 'RV-VSMPE-17' ~ 'CON-VSMPE-5',
                              plot_id == 'RV-VSMPE-13' ~ 'CON-VSMPE-5',
                              plot_id == 'RV-VSMPE-19' ~ 'CON-VSMPE-5',
                              plot_id == 'RV-VSMPE-20' ~ 'CON-VSMPE-4',
                              plot_id == 'RV-VSMPE-21' ~ 'CON-VSMPE-4',
                              plot_id == 'RV-VSMPE-18' ~ 'CON-VSMPE-4',
                              plot_id == 'RV-TR-5' ~ 'CON-TR-3',
                              plot_id == 'RV-TR-9' ~ 'CON-TR-11',
                              plot_id == 'RV-TR-1' ~ 'CON-TR-1',
                              plot_id == 'RV-TR-2' ~ 'CON-TR-1', # CON-TR-2 incomplete dataset
                              plot_id == 'RV-TR-10' ~ 'CON-TR-5', # CON-TR-10 not recovered
                              plot_id == 'RV-TR-8' ~ 'CON-TR-5',
                              plot_id == 'RV-TR-4' ~ 'CON-TR-8',
                              plot_id == 'RV-TR-3' ~ 'CON-TR-13',
                              plot_id == 'RV-TR-13' ~ 'CON-TR-9',
                              plot_id == 'RV-TR-6' ~ 'CON-TR-6',
                              plot_id == 'RV-TR-7' ~ 'CON-TR-7',
                              plot_id == 'RV-TR-12' ~ 'CON-TR-12',
                              plot_id == 'RV-TR-11' ~ 'CON-TR-4')) %>% 
    left_join(., con, by = c('con_id', 'date_time'), suffix = c("", "_con"))
  
  cooling <- rv %>% 
    rowwise() %>% 
    mutate(cooling = temp_C_con - temp_C)
  
  return(cooling)
                              
  }
