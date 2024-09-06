plot_temp_17h <- function(cooling){
  
  t <- cooling %>%
    filter(str_detect(plot_id, 'VSMPE') == T) %>%
    separate(date_time, c("date", "time"), sep = " ") %>%
    filter(time == '17:00:00') %>%
    mutate(type = case_when(str_detect(plot_id, "CON") == T ~ 'Ruelle Traditionelle',
                            str_detect(plot_id, "RV") == T ~ 'Ruelle Verte')) %>%
    select(c(date, type, plot_id, temp_C))
  
  con <- cooling %>%
    filter(str_detect(plot_id, 'VSMPE') == T) %>%
    separate(date_time, c("date", "time"), sep = " ") %>%
    filter(time == '17:00:00') %>%
    group_by(date) %>%
    distinct(con_id, .keep_all = T) %>%
    summarize(temp_C = mean(temp_C_con),
              type ='Ruelle Traditionelle',
              plot_id = 'control_group')
  
  df <- rbind(con, t)
  
  
  p <- ggplot(df, aes(date, temp_C, group = plot_id)) +
    geom_line(data = df[df$type == "Ruelle Verte",], aes(color = "Ruelle Verte"), alpha = 0.2) +
    geom_line(data = df[df$type == "Ruelle Traditionelle",], aes(color = "Moyenne Ruelle\nTraditionelle")) +
    scale_colour_manual(values = c("red", "grey20")) +
    labs(x = "", colour = "", y = "Température à 17h00 (\u00B0C)") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
  ggsave('graphics/temp_17h.png', p, width = 13, height = 8, units = 'in')
  
}