clean_trees <- function(trees_raw){
  
  trees <- trees_raw %>% 
    mutate(Genus = case_when(CommonName == 'Dead' ~ 'Dead',
                             .default = Genus),
           Genus = replace_na(Genus, 'Unknown'),
           Genus = case_when(str_detect(Genus, "Pyrus ") == T ~ "Pyrus",
                             str_detect(Genus, "Populus ") == T ~ "Populus",
                             str_detect(Genus, "Thuja ") == T ~ "Thuja",
                             .default = Genus),
           Species = replace_na(Species, 'sp.'),
           scientific_name = paste(Genus, Species, sep = " "),
           scientific_name = case_when(str_detect(scientific_name, "Malus ") == T ~ "Malus baccata",
                                       str_detect(scientific_name, "Prunus sp.") == T ~ "Prunus virginiana",
                                       str_detect(scientific_name, "Pyrus ") == T ~ "Pyrus domestica",
                                       str_detect(scientific_name, "Rosa sp.") == T ~ "Rosa acicularis",
                                       str_detect(scientific_name, "Morus |Morus sp.") ~ "Morus alba",
                                       str_detect(scientific_name, "Abies sp.") == T ~ "Abies concolor",
                                       str_detect(scientific_name, "Ostrya triacanthos") == T ~ "Ostrya virginiana",
                                       str_detect(scientific_name, "Quercus robur 'Crimson Spire'") == T ~ "Quercus robur 'Fastiagata'",
                                       str_detect(scientific_name, "TIlia cordata") == T ~ "Tilia cordata",
                                       .default = scientific_name),
           type = case_when(str_detect(InfrastructureID, 'RV') == T ~ 'Ruelles Vertes',
                            str_detect(InfrastructureID, 'SS') == T ~ 'Segments des Rues',
                            str_detect(InfrastructureID, 'CON') == T ~ 'Ruelles Traditionelles'),
           city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                            str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières')) %>% 
    drop_na(DBH) 
  
  # Ulmus average between frontier, pumila, 'Morton Glossy'
  
}