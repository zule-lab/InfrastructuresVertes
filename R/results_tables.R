results_tables <- function(model_list, ecological_benefits, ecosystem_services){
  
  # Ecological benefits -----------------------------------------------------
  ecobens_mods <- model_list[c('canopy_vsmpe', 'canopy_tr', 'fireflies_vsmpe', 'sr_vsmpe', 'sr_tr', 
                               'fg_vsmpe', 'fg_tr', 'vc_vsmpe', 'vc_tr', 'pn_vsmpe', 'pn_tr',
                               'pi_vsmpe', 'pi_tr')]
  
  con1 <- lapply(ecobens_mods, function(x){
    x %>%
      emmeans(~ type,
              at = list(type = c('Ruelles Vertes', 'Ruelles Traditionelles', 'Segments des Rues'),
                        per_fr_s = 0,
                        per_en_s = 0,
                        per_no_fren_s = 0,
                        medinc_s = 0,
                        total_trees = 10,
                        n = 10),
              epred = TRUE, re_formula = NULL) %>%
      contrast(method = "pairwise") %>%
      gather_emmeans_draws() %>%
      filter(contrast != "Ruelles Traditionelles - Segments des Rues") %>%
      median_hdi()
  })
  
  tab1 <- bind_rows(con1, .id = "variable") %>%
    separate(variable, c("Model", "City"), "_") %>%
    mutate(City = case_when(str_detect(City, 'vsmpe') == T ~ 'VSMPE',
                            str_detect(City, 'tr') == T ~ 'TR')) %>%
    select(-c(.width, .point, .interval)) %>%
    mutate(.value = case_when(Model == 'canopy' ~ (.value * sd(ecological_benefits$per_can))*100,
                              Model == 'vc' ~ .value * sd(ecological_benefits$avg_complexity),
                              Model == 'pn' ~ .value / 10, # set total_trees = 10 in emmeans above
                              Model == 'pi' ~ .value / 10, # set total_trees = 10 in emmeans above
                              .default = .value),
           .lower = case_when(Model == 'canopy' ~ (.lower * sd(ecological_benefits$per_can))*100,
                              Model == 'vc' ~ .lower * sd(ecological_benefits$avg_complexity),
                              Model == 'pn' ~ .lower / 10, # set total_trees = 10 in emmeans above
                              Model == 'pi' ~ .lower / 10, # set total_trees = 10 in emmeans above
                              .default = .lower),
           .upper = case_when(Model == 'canopy' ~ (.upper * sd(ecological_benefits$per_can))*100,
                              Model == 'vc' ~ .upper * sd(ecological_benefits$avg_complexity),
                              Model == 'pn' ~ .upper / 10, # set total_trees = 10 in emmeans above
                              Model == 'pi' ~ .upper / 10, # set total_trees = 10 in emmeans above
                              .default = .upper),
           # round values
           across(where(is.numeric), ~ round(.x, 4)),
           # assign units
           Units = case_when(Model == 'canopy' ~ '% canopy cover',
                             Model == 'fireflies' ~ 'prob. of firefly presence',
                             Model == 'sr' ~ '# tree species',
                             Model == 'fg' ~ '# functional groups',
                             Model == 'vc' ~ '# avg. levels of complexity',
                             Model == 'pn' ~ 'prop. native tree species',
                             Model == 'pi' ~ 'prop. invasive tree species'
           ),
           # reassign model names
           Model = case_when(Model == 'canopy' ~ 'Canopy Cover',
                             Model == 'fireflies' ~ 'Firefly Presence/Absence',
                             Model == 'sr' ~ 'Tree Species Richness',
                             Model == 'fg' ~ 'Functional Tree Diversity',
                             Model == 'vc' ~ 'Average Vertical Complexity',
                             Model == 'pn' ~ 'Proportion Native Tree Species',
                             Model == 'pi' ~ 'Proportion Invasive Tree Species'
           ),
           # reassign contrast names
           contrast = case_when(contrast == 'Ruelles Vertes - Ruelles Traditionelles' ~ 'Green - Grey',
                                contrast == 'Ruelles Vertes - Segments des Rues' ~ 'Green - Sidewalk')) %>%
    rename(Contrast = contrast,
           `Global Mean` = .value,
           `Lower Credible Interval` = .lower,
           `Higher Credible Interval` = .upper) %>%
    flextable()
  
  
  save_as_docx(tab1, path = 'output/Table1.docx')
  
  
  
  # Ecosystem services ------------------------------------------------------
  
  ecoserv_mods <- model_list[c('ta_vsmpe', 'ta_tr', 'dbh_vsmpe', 'dbh_tr', 'hgt_vsmpe', 'hgt_tr', 'pf_vsmpe', 'pf_tr')]
  
  con1 <- lapply(ecoserv_mods, function(x){
    x %>%
      emmeans(~ type,
              at = list(type = c('Ruelles Vertes', 'Ruelles Traditionelles', 'Segments des Rues'),
                        per_fr_s = 0,
                        per_en_s = 0,
                        per_no_fren_s = 0,
                        medinc_s = 0,
                        total_trees = 10,
                        n = 10),
              epred = TRUE, re_formula = NULL) %>%
      contrast(method = "pairwise") %>%
      gather_emmeans_draws() %>%
      filter(contrast != "Ruelles Traditionelles - Segments des Rues") %>%
      median_hdi()
  })
  
  tab2 <- bind_rows(con1, .id = "variable") %>%
    separate(variable, c("Model", "City"), "_") %>%
    mutate(City = case_when(str_detect(City, 'vsmpe') == T ~ 'VSMPE',
                            str_detect(City, 'tr') == T ~ 'TR')) %>%
    select(-c(.width, .point, .interval)) %>%
    mutate(.value = case_when(Model == 'dbh' ~ .value * sd(ecosystem_services[[2]]$meanDBH, na.rm = T),
                              Model == 'hgt' ~ .value * sd(ecosystem_services[[2]]$mean_pot_hgt, na.rm = T),
                              Model == 'pf' ~ .value / 10, # set n = 10 in emmeans above
                              .default = .value),
           .lower = case_when(Model == 'dbh' ~ .lower * sd(ecosystem_services[[2]]$meanDBH, na.rm = T),
                              Model == 'hgt' ~ .lower * sd(ecosystem_services[[2]]$mean_pot_hgt, na.rm = T),
                              Model == 'pf' ~ .lower / 10, # set n = 10 in emmeans above
                              .default = .lower),
           .upper = case_when(Model == 'dbh' ~ .upper * sd(ecosystem_services[[2]]$meanDBH, na.rm = T),
                              Model == 'hgt' ~ .upper * sd(ecosystem_services[[2]]$mean_pot_hgt, na.rm = T),
                              Model == 'pf' ~ .upper / 10, # set n = 10 in emmeans above
                              .default = .upper),
           # round values
           across(where(is.numeric), ~ round(.x, 4)),
           # assign units
           Units = case_when(Model == 'ta' ~ '# trees',
                             Model == 'dbh' ~ 'mean DBH (cm)',
                             Model == 'hgt' ~ 'mean max height (m)',
                             Model == 'pf' ~ 'prop. flowering trees'
           ),
           # reassign model names
           Model = case_when(Model == 'ta' ~ 'Tree Abundance',
                             Model == 'dbh' ~ 'Mean DBH',
                             Model == 'hgt' ~ 'Mean Maximum Height',
                             Model == 'pf' ~ 'Proportion Flowering Trees'
           ),
           # reassign contrast names
           contrast = case_when(contrast == 'Ruelles Vertes - Ruelles Traditionelles' ~ 'Green - Grey',
                                contrast == 'Ruelles Vertes - Segments des Rues' ~ 'Green - Sidewalk')) %>%
    rename(Contrast = contrast,
           `Global Mean` = .value,
           `Lower Credible Interval` = .lower,
           `Higher Credible Interval` = .upper) %>%
    flextable()
  
  
  save_as_docx(tab2, path = 'output/Table2.docx')
  
}