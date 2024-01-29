read_controls <- function(mtl_path, tr_path, canopy_path){
  
  mtl_path <- download_shp(mtl_path, 'input/roads.zip')
  
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
    group_by(Name) %>%
    summarize(RUELLE_ID = Name, 
           CODE_ARR = "TR",
           geometry = st_union(geometry)) %>% 
    st_buffer(3) %>%
    select(RUELLE_ID, CODE_ARR, geometry)
  
  ctls <- rbind(mtl_controls, tr_controls)
  
  
  return(ctls)
  
}