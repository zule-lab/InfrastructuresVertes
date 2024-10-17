plot_temp_var_can <- function(temp_mit, study_rv, study_controls){
  
  t <- temp_mit %>%
    filter(str_detect(plot_id, 'VSMPE') == T) %>%
    separate(date_time, c("date", "time"), sep = " ") %>%
    group_by(plot_id, date) %>% 
    filter(time == '17:00:00' | time == '23:00:00') %>%
    mutate(temp_diff = temp_C - lag(temp_C, default = first(temp_C), order_by = time),
           type = case_when(str_detect(plot_id, "CON") == T ~ 'Ruelle grise',
                               str_detect(plot_id, "RV") == T ~ 'Ruelle verte')) %>%
    select(c(date, type, plot_id, temp_diff)) %>% 
    filter(temp_diff != 0)
  
  con <- temp_mit %>%
    filter(str_detect(plot_id, 'VSMPE') == T) %>%
    separate(date_time, c("date", "time"), sep = " ") %>%
    filter(time == '17:00:00' | time == '23:00:00') %>% 
    group_by(date, time) %>%
    distinct(con_id, .keep_all = T) %>%
    group_by(con_id, date) %>% 
    mutate(temp_diff = temp_C_con - lag(temp_C_con, default = first(temp_C_con), order_by = time),
           type = 'Ruelle grise') %>% 
    select(c(date, type, con_id, temp_diff)) %>%
    rename(plot_id = con_id) %>% 
    filter(temp_diff != 0)
    
  can <- rbind(study_rv, study_controls)
  
  df <- rbind(con, t) %>% 
    left_join(., can, by = join_by('plot_id' == 'RUELLE_CODE')) %>% 
    mutate(per_can = per_can*100)
  
  p <- ggplot(aes(x = per_can, y = temp_diff, colour = type), data = df) + 
    geom_point() + 
    scale_colour_manual(values = c("darkgrey", "darkgreen")) +
    theme_classic() + 
    labs(x = "Pourcentage de canopée (%)", y = "Changement de températire de 17h00 à 23h00 (\u00B0C)", colour = "",
         title = "Changement de temp. entre 23h et 17h vs. Canopée", subtitle = "a) Villeray-Saint Michel-Parc Extension") + 
    stat_cor(aes(x = per_can, y = temp_diff), method="pearson", label.x = 65, inherit.aes = F) + 
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 12), 
          axis.title = element_text(size = 12))
  
  tr <- temp_mit %>%
    filter(str_detect(plot_id, 'TR') == T) %>%
    separate(date_time, c("date", "time"), sep = " ") %>%
    group_by(plot_id, date) %>% 
    filter(time == '17:00:00' | time == '23:00:00') %>%
    mutate(temp_diff = temp_C - lag(temp_C, default = first(temp_C), order_by = time),
           type = case_when(str_detect(plot_id, "CON") == T ~ 'Ruelle grise',
                            str_detect(plot_id, "RV") == T ~ 'Ruelle verte')) %>%
    select(c(date, type, plot_id, temp_diff)) %>% 
    filter(temp_diff != 0)
  
  con_tr <- temp_mit %>%
    filter(str_detect(plot_id, 'TR') == T) %>%
    separate(date_time, c("date", "time"), sep = " ") %>%
    filter(time == '17:00:00' | time == '23:00:00') %>% 
    group_by(date, time) %>%
    distinct(con_id, .keep_all = T) %>%
    group_by(con_id, date) %>% 
    mutate(temp_diff = temp_C_con - lag(temp_C_con, default = first(temp_C_con), order_by = time),
           type = 'Ruelle grise') %>% 
    select(c(date, type, con_id, temp_diff)) %>%
    rename(plot_id = con_id) %>% 
    filter(temp_diff != 0)
  
  df_tr <- rbind(con_tr, tr) %>% 
    left_join(., can, by = join_by('plot_id' == 'RUELLE_CODE')) %>% 
    mutate(per_can = per_can*100)
  
  p2 <- ggplot(aes(x = per_can, y = temp_diff, colour = type), data = df_tr) + 
    geom_point() + 
    scale_colour_manual(values = c("darkgrey", "darkgreen")) +
    theme_classic() + 
    labs(x = "Pourcentage de canopée (%)", y = "Changement de températire de 17h00 à 23h00 (\u00B0C)", colour = "",
         subtitle = "b) Trois-Rivières") + 
    stat_cor(aes(x = per_can, y = temp_diff), method="pearson", label.x = 65, inherit.aes = F) + 
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 12), 
          axis.title = element_text(size = 12))
  
  
  pt <- p / p2
  
  
  ggsave('graphics/tempdiff_can.png', pt, width = 12, height = 12, units = 'in')
  
  
  
  
}