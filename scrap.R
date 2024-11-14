tar_load(model_list)
tar_load(ecological_benefits)


mod <- model_list$canopy_vsmpe

expand_grid(type = c('Ruelles Vertes', 'Ruelles Traditionelles', 'Segments des Rues'),
            per_fr_s = 0,
            per_en_s = 0,
            per_no_fren_s = 0,
            medinc_s = 0) %>% 
  add_epred_draws(mod, re_formula = NA) %>%
  ggplot(aes(x = .epred, y = type)) +
  stat_slab()


mod <- model_list$canopy_tr

expand_grid(type = c('Ruelles Vertes', 'Ruelles Traditionelles', 'Segments des Rues')) %>% 
  add_epred_draws(mod) %>%
  ggplot(aes(x = .epred, y = type)) +
  stat_slab()

