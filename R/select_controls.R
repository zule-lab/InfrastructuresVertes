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
                            RUELLE_ID == "104006398" | 
                            RUELLE_ID == "104006166" | 
                            RUELLE_ID == "104006387" |
                            RUELLE_ID == "104006131" | 
                            RUELLE_ID == "104006342") %>%
    distinct(RUELLE_ID, .keep_all = T) %>%
    mutate(RUELLE_CODE = paste0("CON-", CODE_ARR, "-", row_number()))
  
  write.csv(rv_f %>% st_set_geometry(NULL), 'results/control-ruelles.csv')
  write_sf(select(rv_f, c(RUELLE_CODE, geometry)), 'results/control-ruelles.kml')
  
  return(rv_f)
  
}