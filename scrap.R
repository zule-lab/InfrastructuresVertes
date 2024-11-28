df <- ecosystem_services[[1]] %>%
  filter(date == '2023-06-28') %>%
  filter(InfrastructureID == "CON-VSMPE-1") %>%
  filter(city == "Villeray-Saint Michel-Parc Extension") %>%
  mutate(time_f = hms(time)) %>% 
  add_epred_rvars(temp_vsmpe_brms_sample)

ggplot(data = df, aes(x = time_f, y = temp_C_s, colour = tod)) +
  stat_lineribbon(aes(dist=.epred)) +
  geom_point()





#tar_load(model_list)
#mod <- model_list$canopy_vsmpe


tar_load(ecological_benefits)



mod <- brm(formula = per_can_s ~ 1 + type + per_fr_s + per_en_s + per_no_fren_s + medinc_s + (1 | Q_socio),
           family = gaussian(),
           prior = c( 
             prior(normal(0, 0.3), class = "b"),
             prior(normal(0, 0.5), class = "Intercept"),
             prior(normal(0, 0.2), class = "sd"),
             prior(exponential(1), class = "sigma")
           ),
           backend = 'cmdstanr',
           data = ecological_benefits %>% filter(city == "Villeray-Saint Michel-Parc Extension"),
           chains = 4,
           iter = 1000,
           cores = 4)




# VSMPE -------------------------------------------------------------------

t <- expand_grid(type = c('Ruelles Vertes', 'Ruelles Traditionelles', 'Segments des Rues'),
                 per_fr_s = 0,
                 per_en_s = 0,
                 per_no_fren_s = 0,
                 medinc_s = 0,
                 Q_socio = c('Villeray', 'Saint-Michel', 'Parc-Extension')) %>% 
  add_epred_draws(mod, re_formula = ~ (1 | Q_socio)) %>%
  ggplot(aes(x = .epred, y = type, fill = Q_socio)) +
  stat_slab(alpha = 0.7) + 
  #scale_fill_manual(values = c("#CFA35E", "#45A291"), labels = c('Day', 'Night')) + 
  theme_classic() + 
  theme(legend.position = "top",
        strip.text.y = element_text(angle = 0),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16)) +
  labs( y = "",
        color = "", 
        fill = "")

# get axis breaks 
atx <- c(as.numeric(na.omit(layer_scales(t)$x$break_positions())))

# unscale x axis 
t + 
  scale_x_continuous(name = "Canopy Cover (%)", 
                     breaks = atx,
                     labels = (round(atx * sd(ecological_benefits$per_can) + mean(ecological_benefits$per_can), 1))*100)



# contrast

ruelle_effect_draws <- mod %>%
  emmeans(~ type,
          at = list(type = c('Ruelles Vertes', 'Ruelles Traditionelles', 'Segments des Rues'),
                    per_fr_s = 0,
                    per_en_s = 0,
                    per_no_fren_s = 0,
                    medinc_s = 0),
          epred = TRUE, re_formula = NA) %>%
  contrast(method = "revpairwise") %>%
  gather_emmeans_draws() %>%
  filter(contrast != "Segments des Rues - Ruelles Traditionelles")



s <- ggplot(ruelle_effect_draws, aes(x = .value, fill = contrast)) +
  stat_halfeye(alpha = 0.7) +
  labs(x = "Average marginal effect of alley type", y = "Density", fill = "") +
  theme_classic() +
  theme(legend.position = "top")

# get axis breaks
satx <- c(as.numeric(na.omit(layer_scales(s)$x$break_positions())))

# unscale x axis
s +
  scale_x_continuous(name = "Average marginal effect of alley type (canopy cover %)",
                     breaks = satx,
                     labels = (round(satx * (sd(ecological_benefits$per_can))*100)))



# TR ----------------------------------------------------------------------

expand_grid(type = c('Ruelles Vertes', 'Ruelles Traditionelles', 'Segments des Rues')) %>% 
  add_epred_draws(mod) %>%
  ggplot(aes(x = .epred, y = type)) +
  stat_slab()

