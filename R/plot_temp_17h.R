plot_temp_17h <- function(cooling){
  
  t <- cooling %>%
    filter(str_detect(plot_id, 'VSMPE') == T) %>%
    separate(date_time, c("date", "time"), sep = " ") %>%
    filter(time == '17:00:00') %>%
    mutate(type = case_when(str_detect(plot_id, "CON") == T ~ 'Ruelle grise',
                            str_detect(plot_id, "RV") == T ~ 'Ruelle verte')) %>%
    select(c(date, type, plot_id, temp_C))
  
  con <- cooling %>%
    filter(str_detect(plot_id, 'VSMPE') == T) %>%
    separate(date_time, c("date", "time"), sep = " ") %>%
    filter(time == '17:00:00') %>%
    group_by(date) %>%
    distinct(con_id, .keep_all = T) %>%
    summarize(temp_C = mean(temp_C_con),
              type ='Ruelle grise',
              plot_id = 'control_group')
  
  df <- rbind(con, t)
  
  
  p <- ggplot(df, aes(date, temp_C, group = plot_id)) +
    geom_line(data = df[df$type == "Ruelle verte",], aes(color = "Ruelle verte"), alpha = 0.2) +
    geom_line(data = df[df$type == "Ruelle grise",], aes(color = "Moyenne ruelle\ngrise")) +
    scale_colour_manual(values = c("red", "grey20")) +
    labs(x = "", colour = "", y = "Température à 17h00 (\u00B0C)", title = "a) Villeray-Saint Michel-Parc Extension") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 12), 
          axis.title = element_text(size = 12))
  
  
  tr <- cooling %>%
    filter(str_detect(plot_id, 'TR') == T) %>%
    separate(date_time, c("date", "time"), sep = " ") %>%
    filter(time == '17:00:00') %>%
    mutate(type = case_when(str_detect(plot_id, "CON") == T ~ 'Ruelle grise',
                            str_detect(plot_id, "RV") == T ~ 'Ruelle verte')) %>%
    select(c(date, type, plot_id, temp_C))
  
  con_tr <- cooling %>%
    filter(str_detect(plot_id, 'TR') == T) %>%
    separate(date_time, c("date", "time"), sep = " ") %>%
    filter(time == '17:00:00') %>%
    group_by(date) %>%
    distinct(con_id, .keep_all = T) %>%
    summarize(temp_C = mean(temp_C_con),
              type ='Ruelle grise',
              plot_id = 'control_group')
  
  df_tr <- rbind(con_tr, tr)
  
  
  p2 <- ggplot(df_tr, aes(date, temp_C, group = plot_id)) +
    geom_line(data = df_tr[df_tr$type == "Ruelle verte",], aes(color = "Ruelle verte"), alpha = 0.2) +
    geom_line(data = df_tr[df_tr$type == "Ruelle grise",], aes(color = "Moyenne ruelle\n grise")) +
    scale_colour_manual(values = c("red", "grey20")) +
    labs(x = "", colour = "", y = "Température à 17h00 (\u00B0C)", title = "b) Trois-Rivières") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 12), 
          axis.title = element_text(size = 12))
  
  
  pt <- p / p2 
  
  ggsave('graphics/temp_17h.png', pt, width = 13, height = 13, units = 'in')
  
  
}