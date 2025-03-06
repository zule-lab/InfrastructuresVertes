calc_census <- function(da_raw, census_raw, study_rv, study_controls, can_cov_street){
  
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
  
  da_area <- da_raw %>% 
    mutate(area_total = st_area(geometry))
  
  study_da <- st_transform(study, st_crs(da_raw)) %>% 
    st_intersection(da_area) %>% 
    mutate(DAUID = as.integer(DAUID),
           areaint = st_area(geometry)) # area of the DA that intersects w the buffer (sq m)
  
  census_da_f <- census_raw %>%
    select(c("ALT_GEO_CODE","CHARACTERISTIC_ID","C1_COUNT_TOTAL")) %>%
    rename(DAUID = "ALT_GEO_CODE",
           sofac = "CHARACTERISTIC_ID",
           sonum = "C1_COUNT_TOTAL") %>%
    right_join(study_da, by = "DAUID") %>%
    filter(sofac %in% c(1, 6:7, 115, 384:387, 1527:1529, 1544, 1545, 1557, 1574, 1585, 1603, 2014:2017)) 
  
  # we are interested in education, language, income, and ethnicity 
  
  # 1   Population 2021
  # 6   Pop density per sq km
  # 7   Land area sq km
  
  # Income statistics in 2020 for the population aged 15 years and over in private households - 100% data (10)
  # 115   Median after-tax income in 2020 among recipients ($)
  
  # Knowledge of official languages for the total population excluding institutional residents - 100% data (36)
  # 384	  English only
  # 385	  French only
  # 386	  English and French
  # 387	  Neither English nor French
  
  # 1527    Total - Immigrant status and period of immigration for the population in private households - 25% sample data (79)
  # 1528	  Non-immigrants (80)
  # 1529	  Immigrants (81)
  
  # 1544    Total - Place of birth for the immigrant population in private households - 25% sample data (85)
  # 1545	  Americas
  # 1557	  Europe
  # 1574	  Africa
  # 1585	  Asia
  # 1603	  Oceania and other places of birth (93)
  
  # 2014    Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data (165)
  # 2015	  No certificate, diploma or degree
  # 2016	  High (secondary) school diploma or equivalency certificate (167)
  # 2017	  Postsecondary certificate, diploma or degree
  
  
  census_da_w <- census_da_f %>% pivot_wider(names_from = sofac, values_from = sonum)
  
  census_da_r <- census_da_w %>%
    rename(totpop = "1") %>%
    rename(popdens = "6") %>%
    rename(area = "7") %>%
    rename(medinc = "115") %>% 
    rename(en = "384") %>% 
    rename(fr = "385") %>% 
    rename(fr_en = "386") %>% 
    rename(no_fr_en = "387") %>% 
    rename(tot_imm = "1527") %>% 
    rename(non_imm = "1528") %>% 
    rename(imm = "1529") %>% 
    rename(tot_orig = "1544") %>% 
    rename(imm_amer = "1545") %>% 
    rename(imm_eur = "1557") %>% 
    rename(imm_afr = "1574") %>% 
    rename(imm_asia = "1585") %>% 
    rename(imm_oth = "1603") %>% 
    rename(tot_edu = "2014") %>% 
    rename(edu_no = "2015") %>% 
    rename(edu_sec = "2016") %>%
    rename(edu_postsec = "2017")
  
  census_da_sf <- st_as_sf(census_da_r, sf_column_name = c("geometry"), crs = st_crs(study_da))
  
  census_da_na <- census_da_sf %>% 
    drop_na(InfrastructureID)  %>%
    filter(area > 0) %>%
    mutate(da = as.factor(DAUID)) %>%
    mutate(across(c(totpop:edu_postsec), ~as.numeric(.))) %>%
    select(-c(DGUID, LANDAREA, PRUID, DAUID))
  
  # population percentages
  can_cen_pp <- census_da_na %>% 
    mutate(per_en = en/totpop,
           per_fr = fr/totpop,
           per_fren = fr_en/totpop,
           per_no_fren = no_fr_en/totpop,
           per_non_imm = non_imm/tot_imm,
           per_imm = imm/tot_imm,
           per_amer = imm_amer/tot_orig,
           per_eur = imm_eur/tot_orig,
           per_afr = imm_afr/tot_orig,
           per_asia = imm_asia/tot_orig, 
           per_oth = imm_oth/tot_orig, 
           per_edu_no = edu_no/tot_edu,
           per_edu_sec = edu_sec/tot_edu,
           per_edu_postsec = edu_postsec/tot_edu)
  
  
  # need to calculate the area of the DA that is within the neigbourhood (areaint)
  study_cen_pop <- can_cen_pp %>%
    # to calculate the approximate population within the neighbourhood bounds (assuming equal density throughout the DA)
    # divide the intersected area/total area of DA and multiply the population by that 
    # can then use this population as weight for weighted means
    mutate(popwithin = (as.numeric(areaint)/as.numeric(area_total))*as.numeric(totpop)) %>% 
    select(c("InfrastructureID","da","geometry","totpop", "popwithin", "popdens", "area", "area_total", "areaint",
             "medinc", "per_en", "per_fr", "per_fren", "per_no_fren", "per_non_imm", "per_imm", 
             "per_amer", "per_eur", "per_afr", "per_asia", "per_oth", 
             "per_edu_no", "per_edu_sec", "per_edu_postsec"))
  
  # population weighted mean
  study_cen <- study_cen_pop %>%
    group_by(InfrastructureID) %>%   
    summarize(DAcount = n(),
              totarea = sum(areaint),
              geometry = st_union(geometry),
              popdens = weighted.mean(as.numeric(popdens), as.numeric(popwithin), na.rm = T),
              per_en = weighted.mean(as.numeric(per_en), as.numeric(popwithin), na.rm = T),
              per_fr = weighted.mean(as.numeric(per_fr), as.numeric(popwithin), na.rm = T),
              per_fren = weighted.mean(as.numeric(per_fren), as.numeric(popwithin), na.rm = T),
              per_no_fren = weighted.mean(as.numeric(per_no_fren), as.numeric(popwithin), na.rm = T),
              per_non_imm = weighted.mean(as.numeric(per_non_imm), as.numeric(popwithin), na.rm = T),
              per_imm = weighted.mean(as.numeric(per_imm), as.numeric(popwithin), na.rm = T),
              per_amer = weighted.mean(as.numeric(per_amer), as.numeric(popwithin), na.rm = T),
              per_eur = weighted.mean(as.numeric(per_eur), as.numeric(popwithin), na.rm = T),
              per_afr = weighted.mean(as.numeric(per_afr), as.numeric(popwithin), na.rm = T),
              per_asia = weighted.mean(as.numeric(per_asia), as.numeric(popwithin), na.rm = T),
              per_oth = weighted.mean(as.numeric(per_oth), as.numeric(popwithin), na.rm = T),
              per_edu_no = weighted.mean(as.numeric(per_edu_no), as.numeric(popwithin), na.rm = T),
              per_edu_sec = weighted.mean(as.numeric(per_edu_sec), as.numeric(popwithin), na.rm = T),
              per_edu_postsec = weighted.mean(as.numeric(per_edu_postsec), as.numeric(popwithin), na.rm = T),
              medinc = weighted.mean(as.numeric(medinc), as.numeric(popwithin), na.rm = T),
              popwithin = sum(as.numeric(popwithin))
    ) %>%
    distinct(InfrastructureID, .keep_all = TRUE)
  
  return(study_cen)
  
  
}