create_temp_fig <- function(temp_vsmpe_brms_sample, temp_tr_brms_sample, ecosystem_services){
  
  df_tr <- ecosystem_services[[1]] %>% 
    filter(city == "Trois-Rivières") 
  
  df_vsmpe <- ecosystem_services[[1]] %>%
    filter(city == "Villeray-Saint Michel-Parc Extension")
  
  tr_day <- plot_day(temp_tr_brms_sample, df_tr) + ggtitle('Trois-Rivières')
  
  vsmpe_day <- plot_day(temp_vsmpe_brms_sample, df_vsmpe) + ggtitle('Villeray-Saint Michel-Parc Extension')

  tr_tod <- plot_tod(temp_tr_brms_sample, df_tr)
  
  vsmpe_tod <- plot_tod(temp_vsmpe_brms_sample, df_vsmpe)
  
  full <- (vsmpe_day + tr_day) / (vsmpe_tod + tr_tod) 
  
  ggsave('graphics/tempfig.png', full, width = 10, height = 10, units = 'in')
  
  return(full)
  
}

plot_day <- function(mod, df){
  
  s <- df %>%
    data_grid(type, 
              tod, 
              doy,
              per_fr_s = 0,
              per_en_s = 0,
              per_no_fren_s = 0,
              medinc_s = 0) %>%
    add_epred_draws(mod, re_formula = NA) %>%
    ggplot() + 
    geom_point(aes(x = doy, y= temp_C_s), colour = "grey50", alpha = 0.2, data = df) + 
    stat_lineribbon(aes(x = doy, y = .epred, colour = type, fill = type), .width = 0.95, alpha = 0.7) + 
    labs(fill = "", colour = "", x = "Day of sampling") + 
    scale_colour_manual(values = c('grey30', 'darkgreen'), labels = c("Grey alleys", "Green alleys")) + 
    scale_fill_manual(values = c('grey30', 'darkgreen'), labels = c("Grey alleys", "Green alleys")) + 
    theme_classic() + 
    theme(legend.position = 'top')
  
  
  # get axis breaks
  saty <- c(as.numeric(na.omit(layer_scales(s)$y$break_positions())))
  
  # unscale x axis
  tr_day <- s +
    scale_y_continuous(name = "Temperature (\u00B0C)",
                       breaks = saty,
                       labels = (round(saty * sd(df$temp_C) + mean(df$temp_C), 1)))
  
  return(tr_day)
  
  
}

plot_tod <- function(mod, df){
  
  
  ruelle_effect_draws <- mod %>%
    emmeans(~ type:tod,
            at = list(type = c('Ruelles Vertes', 'Ruelles Traditionelles'),
                      tod = c('day', 'night'),
                      per_fr_s = 0,
                      per_en_s = 0,
                      per_no_fren_s = 0,
                      medinc_s = 0),
            epred = TRUE, re_formula = NA) %>%
    contrast(method = "pairwise") %>%
    gather_emmeans_draws() %>% 
    filter(contrast == "Ruelles Vertes day - Ruelles Traditionelles day" |
             contrast == "Ruelles Vertes night - Ruelles Traditionelles night")
  
  
  
  s <- ggplot(ruelle_effect_draws, aes(x = .value, fill = contrast)) +
    stat_halfeye(alpha = 0.7) +
    scale_fill_manual(values = c("darkgoldenrod", "mediumpurple4"), labels = c("Green alleys: day", "Green alleys: night")) + 
    labs(y = "Density", fill = "") +
    theme_classic() +
    theme(legend.position = "top")
  
  # get axis breaks
  satx <- c(as.numeric(na.omit(layer_scales(s)$x$break_positions())))
  
  # unscale x axis
  tod <- s +
    scale_x_continuous(name = "Marginal effect of alley type (\u00B0C)",
                       breaks = satx,
                       labels = (round(satx * (sd(df$temp_C)), 1)))
  
  
  return(tod)
  
  
  
  
}
