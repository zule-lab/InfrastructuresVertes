read_controls <- function(mtl_path, tr_path, canopy_path){
  
  mtl_controls <- read_sf(mtl_path) %>%
    filter(CATEGORIEC == "Ruelle" & PROPRIETAI == "Villeray - St-Michel - Parc-Extension") %>%
    mutate(RUELLE_ID = ID_VOI_VOI,
           CODE_ARR = "VSMPE") %>%
    select(RUELLE_ID, CODE_ARR, geometry) %>%
    st_transform(crs = st_crs(read_stars(canopy_path)))
  
  tr <- read_sf(tr_path, layer = 'ruelles')
  tr_rv <- read_sf(tr_path, layer = 'ruelles vertes') %>% 
    st_set_geometry(NULL)
  tr_controls <- anti_join(tr, tr_rv, by = 'Name') %>% 
    st_transform(st_crs(mtl_controls)) %>%
    select(Name, geometry) %>%
    summarize(RUELLE_ID = Name, 
           CODE_ARR = "TR")
  
  ctls <- rbind(mtl_controls, tr_controls)
  
  
  return(ctls)
  
}