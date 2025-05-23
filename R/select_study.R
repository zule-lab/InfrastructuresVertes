select_study <- function(can_cov_rv_bind, quartiers) {
  
  # intersect with neighbourhoods
  q_t <- st_transform(quartiers, crs = st_crs(can_cov_rv_bind)) %>%
    select(c(Q_socio, geometry))
  
  rv_q <- st_intersection(can_cov_rv_bind, q_t)
  rv_q$RUELLE_ID <- base::gsub('\n', '', rv_q$RUELLE_ID, fixed = TRUE)
  
  # manually select mtl study site ruelles 
  rv_f <- rv_q %>% filter(RUELLE_ID == "VSMPE-I-1300015"|
                            RUELLE_ID == "VSMPE-I-1300005"|
                            RUELLE_ID == "Ruelle verte 2015 : Jean-Talon/Louis-Hébert/Iberville/Everett"|
                            RUELLE_ID == "Ruelle des Érables" |
                            RUELLE_ID == "VSMPE-I-1300014" | 
                            RUELLE_ID == "VSMPE-A-1100015" | 
                            RUELLE_ID == "Ruelle Saint-André/Gounod" | 
                            RUELLE_ID == "Ruelle Saint André/ Gounod" |
                            RUELLE_ID == "VSMPE-T-1100022" | 
                            RUELLE_ID == "Ruelle La-Rose-des-vents" | 
                            RUELLE_ID == "Ruelle des Voisins Verts Villeray" |
                            RUELLE_ID == "Ruelle Les amis de Villeray" | 
                            RUELLE_ID == "VSMPE-I-1100016" |
                            RUELLE_ID == "VSMPE-I-1100004" | 
                            RUELLE_ID == "VSMPE-I-1100008" | 
                            RUELLE_ID == "VSMPE-I-1100019" | 
                            RUELLE_ID == "VSMPE-J-1100012" | 
                            RUELLE_ID == "Ruelle des Amélanchiers" |
                            RUELLE_ID == "Ruelle soleil" | 
                            RUELLE_ID == "Ruelle Papineau/Cartier/Bélanger/Jean-Talon" | 
                            RUELLE_ID == "Ruelle 2e avenue/6e avenue/Jean-Talon/Everett" | 
                            RUELLE_ID == "VSMPE-A-1100029" | 
                            RUELLE_ID == "Ruelle les colibris" | 
                            RUELLE_ID == "Ruelle Bloomfield D'anvers" | 
                            RUELLE_ID == "Ruelle CWH" | 
                            RUELLE_ID == "VSMPE-U-1200015" | 
                            RUELLE_ID == "Ruelle d'Émilie" | 
                            RUELLE_ID == "Ruelle Querbes" | 
                            RUELLE_ID == "Ruelle Parc-Xquenda" | 
                            RUELLE_ID == "Ruelle Ogilvy - Jean-Talon" | 
                            RUELLE_ID == "Ruelle Rita" | 
                            RUELLE_ID == "VSMPE-H-1200013" | 
                            RUELLE_ID == "Ruelle Projet condo derrière le Tim Horton" | 
                            RUELLE_ID == "VSMPE-I-1400003" | 
                            RUELLE_ID == "VSMPE-I-1400002" | 
                            RUELLE_ID == "VSMPE-H-1400001" | 
                            RUELLE_ID == "VSMPE-I-1410005"| 
                            RUELLE_ID == "VSMPE-I-1410006" | 
                            RUELLE_ID == "VSMPE-I-1410004" | 
                            RUELLE_ID == "VSMPE-L-1410003") %>%
    group_by(RUELLE_ID) %>% 
    summarize(CODE_ARR = first(CODE_ARR),
              ruelle_area = sum(ruelle_area),
              canopy = mean(canopy), 
              per_can = mean(per_can),
              Q_socio = first(Q_socio),
              geometry = st_union(geometry)) %>% 
    mutate(RUELLE_CODE = paste0("RV-", CODE_ARR, "-", row_number()))
  
  # manually select TR study site ruelles 
  rv_tr <- can_cov_rv_bind %>% filter(CODE_ARR == "TR") %>%
    distinct(RUELLE_ID, .keep_all = T) %>%
    mutate(RUELLE_CODE = paste0("RV-", CODE_ARR, "-", row_number()),
           Q_socio = "Trois-Rivières")
  
  # bind mtl and tr ruelles 
  rv <- rbind(rv_f, rv_tr)
  
  # save
  write.csv(rv %>% st_set_geometry(NULL), 'output/study-ruelles.csv')
  write_sf(select(rv, c(RUELLE_CODE, geometry)), 'output/study-ruelles.kml')
  
  return(rv)
  
}