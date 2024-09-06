plot_temp_can <- function(temp_mit, study_rv, study_controls){
  
  t <- temp_mit %>%
    filter(str_detect(plot_id, 'VSMPE') == T) %>%
    separate(date_time, c("date", "time"), sep = " ") %>%
    group_by(plot_id, date, tod) %>%
    summarize(mean_cooling = mean(cooling), na.rm = T)
  
  
  # filter tree div dataset for only ruelles vertes
  ecological_benefits <- rbind(study_rv, study_controls)
  
  can <- ecological_benefits %>%
    filter(str_detect(RUELLE_CODE, 'RV-VSMPE') == T) %>%
    slice_max(per_can, n = 3) %>%
    select(c(RUELLE_CODE, per_can)) %>%
    mutate(per_can = as.factor(round(per_can*100, 1)))
  
  # join with temperature data
  max_can <- inner_join(t, can, by = join_by(plot_id == RUELLE_CODE))
  
  time <- c(
    day = "Villeray-Saint Michel-Parc Extension, Jour",
    night = "Villeray-Saint Michel-Parc Extension, Nuit"
  )
  
  ggplot(t, aes(date, mean_cooling, group = plot_id)) +
    geom_line(colour = "grey20", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = 2, color = "black", linewidth = 1 ) +
    geom_line(data = max_can, aes(colour = per_can), linewidth = 1) +
    scale_colour_manual(values = c("#9cc184","#1f5b25", "#192813")) +
    labs(x = "", colour = "Pourcentage de \ncanopée (%)", y = "Effet de refroidissement (\u00B0C)") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text = element_text(size = 16, color = 'black'),
          axis.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          strip.text = element_text(size = 16)) +
    facet_wrap(vars(tod), labeller = labeller(tod = as_labeller(time)))
  
  ggsave('graphics/tempcan_tradeoff.png', width = 13, height = 8, units = 'in')
  
}