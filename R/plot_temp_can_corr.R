plot_temp_can_corr <- function(temp_mit, study_rv, study_controls){
  
  t <- temp_mit %>%
    filter(str_detect(plot_id, 'VSMPE') == T) %>%
    separate(date_time, c("date", "time"), sep = " ") %>%
    mutate(type = case_when(str_detect(plot_id, "CON") == T ~ 'Ruelle traditionelle',
                            str_detect(plot_id, "RV") == T ~ 'Ruelle verte')) %>%
    select(c(date, time, type, plot_id, temp_C)) 
  
  con <- temp_mit %>%
    filter(str_detect(plot_id, 'VSMPE') == T) %>%
    separate(date_time, c("date", "time"), sep = " ") %>%
    group_by(date, time) %>%
    distinct(con_id, .keep_all = T) %>%
    mutate(type = 'Ruelle traditionelle') %>% 
    select(c(date, time, type, con_id, temp_C_con)) %>%
    rename(plot_id = con_id,
           temp_C = temp_C_con)
  
  can <- rbind(study_rv, study_controls)
  
  df <- rbind(con, t) %>% 
    left_join(., can, by = join_by('plot_id' == 'RUELLE_CODE')) %>% 
    mutate(per_can = per_can*100)
  
  p <- ggplot(aes(x = per_can, y = temp_C, colour = type), data = df) + 
    geom_point() + 
    scale_colour_manual(values = c("darkgrey", "darkgreen")) +
    theme_classic() + 
    labs(x = "Pourcentage de canopée (%)", y = "Température (\u00B0C)", colour = "") + 
    stat_cor(aes(x = per_can, y = temp_C), method="pearson", label.x = 62, inherit.aes = F)
  
  ggsave('graphics/temp_can_corr.png', p, width = 8, height = 8, units = 'in')
  
}