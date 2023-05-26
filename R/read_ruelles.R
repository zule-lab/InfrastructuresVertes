read_ruelles <- function(mtl_rv, req_rv, tr_rv, crs) {

# ruelles from Ville de Montreal ------------------------------------------
  
  mtl_rv <- read_sf(mtl_rv) %>% 
    # filter for the neighbourhood
    filter(CODE_ARR == "VSMPE") %>%
    # select relevant columns
    select(RUELLE_ID, CODE_ARR, geometry) %>%
    st_transform(st_crs(crs)) %>% 
    # make 5 m buffer to ensure edges of ruelles are captured
    st_buffer(5)


# Trois-Rivieres ruelles --------------------------------------------------

  tr_rv <- read_sf(tr_rv) %>% 
    st_transform(st_crs(crs)) %>%
    # 8 m buffer because linestring - 3 m for width of ruelle and then 5 m to get to edges
    st_buffer(8) %>%
    mutate(RUELLE_ID = Name,
           CODE_ARR = "TR") %>%
    select(RUELLE_ID, CODE_ARR, geometry)
  

# ruelles from REQ --------------------------------------------------------
  
  req_vsmpe <- read_sf(req_rv) %>%
    filter(Name == "Arrondissement Villeray-St-Michel-Parc-Extention") %>%
    st_cast("POLYGON") %>%
    st_make_valid()
  
  req_ruelles <- read_sf(req_rv) %>%
    filter(str_detect(Name, "Ruelle"))
  
  req_rv_vsmpe <- st_intersection(req_ruelles, req_vsmpe) %>% 
    st_transform(st_crs(crs)) %>%
    # 8 m buffer because linestring - 3 m for width of ruelle and then 5 m to get to edges
    st_buffer(8) %>%
    mutate(RUELLE_ID = Name,
           CODE_ARR = "VSMPE") %>%
    select(RUELLE_ID, CODE_ARR, geometry)
  
# bind --------------------------------------------------------------------
  rv_tot <- rbind(mtl_rv, req_rv_vsmpe, tr_rv)
  
  return(rv_tot)
  
}