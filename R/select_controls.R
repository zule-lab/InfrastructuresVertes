select_controls <- function(can_cov_controls_bind, quartiers) {
  
  # intersect with neighbourhoods
  q_t <- st_transform(quartiers, crs = st_crs(can_cov_controls_bind)) %>%
    select(c(Q_socio, geometry))
  
  rv_q <- st_intersection(can_cov_controls_bind, q_t)
  
  # manually select study site ruelles 
  rv_f <- rv_q %>% filter(RUELLE_ID == "104006133"|
                            RUELLE_ID == "104006226"|
                            RUELLE_ID == "104006290"|
                            RUELLE_ID == "104006182" |
                            RUELLE_ID == "104006389" | 
                            RUELLE_ID == "104006429" | 
                            RUELLE_ID == "104006166" | 
                            RUELLE_ID == "200438765" |
                            RUELLE_ID == "104006131" | 
                            RUELLE_ID == "104006376") %>%
    distinct(RUELLE_ID, .keep_all = T) %>%
    mutate(RUELLE_CODE = paste0("CON-", CODE_ARR, "-", row_number()))
  
  rv_tr <- can_cov_controls_bind %>% 
    filter(
      RUELLE_ID == "Godbout-Jutras-1" | 
        RUELLE_ID == "Godbout-Jutras-2" | 
        RUELLE_ID == "Wiliams-Gingras-2" | 
        RUELLE_ID == "Caron-St.Paul" | 
        RUELLE_ID == "Wolfe-Amherst-2" | 
        RUELLE_ID == "Hon.Mercier-Chapleau" | 
        RUELLE_ID == "Montacalm-Wolfe" |
        RUELLE_ID == "Ste.Angele-Ste.Cecile-1" | 
        RUELLE_ID == "Ste.Angele-Ste.Cecile-4" | 
        RUELLE_ID == "Brebeuf-Dumoulin" | 
        RUELLE_ID == "2eAv-3eAv" | 
        RUELLE_ID == "5eAv-6eAv" | 
        RUELLE_ID == "Cloutier-Richard") %>%
    distinct(RUELLE_ID, .keep_all = T) %>%
    mutate(Q_socio = "Trois-Rivieres", 
           RUELLE_CODE = paste0("CON-", CODE_ARR, "-", row_number()))
  
  ctrls <- rbind(rv_f, rv_tr)
  
  write.csv(ctrls %>% st_set_geometry(NULL), 'output/control-ruelles.csv')
  write_sf(select(ctrls, c(RUELLE_CODE, geometry)), 'output/control-ruelles.kml')
  
  return(ctrls)
  
}