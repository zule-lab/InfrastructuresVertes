plot_canopy_cov <- function(study_rv, study_controls, can_cov_street){
  
  # canopy cover
  can_cov_street <- can_cov_street %>% 
    mutate(CODE_ARR = case_when(str_detect(Name, 'VSMPE') == T ~ 'VSMPE', 
                                str_detect(Name, 'TR') == T ~ 'TR'),
           Q_socio = NA) %>% 
    rename(InfrastructureID = "Name",
           ruelle_area = "street_area")
  
  study_rv <- study_rv %>% select(-RUELLE_ID) %>% rename(InfrastructureID = "RUELLE_CODE")
  study_controls <- study_controls %>% select(-RUELLE_ID) %>% rename(InfrastructureID = "RUELLE_CODE")
  
  cancov <- rbind(can_cov_street, study_rv, study_controls) %>% select(-Q_socio) %>% 
    mutate(per_can = per_can*100, 
           type = case_when(str_detect(InfrastructureID, 'RV') == T ~ 'Ruelles vertes',
                            str_detect(InfrastructureID, 'SS') == T ~ 'Segments des rues',
                            str_detect(InfrastructureID, 'CON') == T ~ 'Ruelles grises'),
           city = case_when(str_detect(InfrastructureID, 'VSMPE') == T ~ 'Villeray-Saint Michel-Parc Extension',
                            str_detect(InfrastructureID, 'TR') == T ~ 'Trois-Rivières'))
  
  
  plot <- ggplot(cancov, aes(x = type, y = per_can, colour = city)) + 
    geom_boxplot() + 
    geom_point(position=position_jitterdodge()) + 
    scale_colour_manual(values = c("#6e948c", "#122c43")) +
    theme_classic() + 
    labs(colour = "", x = "", y = "Couverture de la canopée (%)") + 
    theme(legend.position = "top",
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  
  
  ggsave('graphics/canopycover.png', plot, height = 10, width = 12, units = 'in')
  
}