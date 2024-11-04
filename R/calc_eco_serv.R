calc_eco_serv <- function(temp_dfs, tr_temp_dfs, study_rv, study_controls,
                          can_cov_street, trees_clean, tree_traits, census_data,
                          quartiers){
  
  # study sites 
  can_cov_street <- can_cov_street %>% 
    rename(InfrastructureID = "Name",
           ruelle_area = "street_area")
  
  study_rv <- study_rv %>% 
    select(-c(RUELLE_ID, CODE_ARR, Q_socio)) %>% 
    rename(InfrastructureID = "RUELLE_CODE")
  
  study_controls <- study_controls %>% 
    select(-c(RUELLE_ID, CODE_ARR, Q_socio)) %>% 
    rename(InfrastructureID = "RUELLE_CODE")
  
  study <- rbind(study_controls, study_rv, can_cov_street)
  
  
  # temperature mitigation
  # make sure ruelle names are formatted correctly, select relevant columns, rename columns, and remove first line of df (units)
  temp_names <- temp_dfs %>% 
    mutate(plot_id = str_replace(plot_id, "CON", "CON-VSMPE-"),
           plot_id = str_replace(plot_id, "RV", "RV-VSMPE-"))
  
  # join temp data with site info 
  temp_vsmpe_geom <- left_join(temp_names, study, by = join_by(plot_id == InfrastructureID))
  temp_tr_geom <- left_join(tr_temp_dfs, study, by = join_by(plot_id == InfrastructureID))
  
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
  
  
  # trees 
  tree_abund <- trees_clean %>% 
    mutate(Genus = case_when(CommonName == 'Dead' ~ 'Dead',
                             .default = Genus),
           Genus = replace_na(Genus, 'Unknown'),
           Species = replace_na(Species, 'sp.'),
           scientific_name = paste(Genus, Species, sep = " ")) %>% 
    group_by(InfrastructureID) %>% 
    summarize(nTrees = n(), 
              type = first(type), 
              city = first(city)) %>% 
    right_join(study, by = "InfrastructureID") %>% 
    # missing ruelles with 0 trees
    mutate(nTrees = if_else(is.na(nTrees), 0, nTrees))
  
  # large trees - DBH and max height
  
  # replace commas with decimals 
  trees_clean$DBH <- gsub(",", ".", trees_clean$DBH, fixed = T)
  # replace plus signs with spaces 
  trees_clean$DBH <- gsub("+", " ", trees_clean$DBH, fixed = T)
  
  # split multiple stem DBH into separate columns 
  ind_dbh <- as.data.frame(str_split_fixed(trees_clean$DBH, " ", 20)) %>% 
    mutate_all(as.numeric) %>% # convert to numeric 
    # substitute NAs w zeroes and square DBH values
    mutate(across(where(is.double), ~ ifelse(is.na(.x), 0, .x)),
           across(where(is.double), ~ (.x)^2),
           DBHCalc = round(sqrt(rowSums(across(where(is.double)))), 1)) %>% 
    select(DBHCalc)
  
  # observed mean DBH 
  trees_dbh <- cbind(trees_clean, ind_dbh) %>% 
    group_by(InfrastructureID) %>% 
    reframe(meanDBH = round(mean(DBHCalc), 1)) %>% 
    right_join(tree_abund)
  
  # potential mean DBH 
  # flowers - showy flowering trees 
  trees_pot_hgt <- left_join(trees_clean, tree_traits, by = join_by('scientific_name' == 'latin.name')) %>% 
    group_by(InfrastructureID) %>% 
    mutate(flowering = case_when(scientific_name == "Rosa acicularis" ~ "showy",
                                 .default = flowering)) %>% 
    drop_na(flowering) %>% 
    summarize(mean_pot_hgt = mean(maximum.height),
              n = n(), 
              showy_count = sum(ifelse(flowering == "showy", 1, 0)),
              prop_showy = showy_count/n) %>% 
    right_join(trees_dbh)
  
  census_trees <- inner_join(trees_pot_hgt, census_data, by = "InfrastructureID")
  
  # greenness (not possible?)
  
  # management (not possible?)
  
  # food provisioning (not possible?)
  
  
  # scale numeric vars & assign type/city to missing rows 
  census_trees_s <- census_trees %>% 
    mutate(type = case_when(str_detect(InfrastructureID, 'RV') == T ~ 'Ruelles Vertes',
                            str_detect(InfrastructureID, 'SS') == T ~ 'Segments des Rues',
                            str_detect(InfrastructureID, 'CON') == T ~ 'Ruelles Traditionelles'),
           city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                            str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières'), 
           across(where(is.numeric), ~ scale(.x)[,1], .names = "{.col}_s"))
  
  
  # neighbourhood
  nhood <- census_trees_s %>% 
    st_as_sf() %>% 
    st_transform(st_crs(quartiers)) %>% 
    st_join(., quartiers, by= st_intersection, left = T)  %>% 
    select(-c(geometry.y, id, Arrondisse, Abrev, nbr_RUI, Table)) %>% 
    # one ruelle slightly crosses over into Saint-Michel so returns duplicate but it isn't truly in SM
    filter(!(InfrastructureID == "RV-VSMPE-20" & Q_socio == "Saint-Michel"))
  
  
  # join cooling w other variables of interest & scale
  temp_join <- left_join(temp_tod, nhood, by = join_by("plot_id" == "InfrastructureID")) %>% 
    rename(InfrastructureID = plot_id) %>%
    drop_na(temp_C) %>% 
    mutate(temp_C_s = scale(temp_C)[,1])  %>% 
    separate(date_time, c("date", "time"), sep = " ")
  
  # save
  es <- list(temp_join, nhood) 
  names(es) <- c("temperature", "other_es")
  
  return(es)
  
  
}