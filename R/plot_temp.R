plot_temp <- function(temp_dfs, tr_temp_dfs) {
  
  
  # Montreal
  mtl_temp_day <- calc_temp(temp_dfs) %>% 
    filter(date > '2023-06-26' & date < '2023-07-28',
           daytime == "day")
  
  mtl_temp_night <- calc_temp(temp_dfs) %>% 
    filter(date > '2023-06-26' & date < '2023-07-28',
           daytime == "night")
  
  mtl_day <- plot_ind(mtl_temp_day, 'Montréal, jour')
  mtl_night <- plot_ind(mtl_temp_night, 'Montréal, nuit')
  
  legend <- get_legend(mtl_day)
  mtl <- plot_grid(plot_grid(mtl_day + theme(legend.position = 'none'),
                             mtl_night + theme(legend.position = 'none'), align = 'h'),
                   legend,
                   nrow = 1, rel_widths = c(6,1))
  
  ggsave('graphics/mtltemp.png', mtl, width = 10, height = 6, units = 'in', dpi = 450)
  
  
  # Trois-Rivieres
    
  tr_temp_day <- calc_temp(tr_temp_dfs) %>% 
    filter(date > '2023-08-08' & date < '2023-10-01',
           daytime == "day")
  
  tr_temp_night <- calc_temp(tr_temp_dfs) %>% 
    filter(date > '2023-08-08' & date < '2023-10-01',
           daytime == "night")
  
  tr_day <- plot_ind(tr_temp_day, 'Trois-Rivières, jour')
  tr_night <- plot_ind(tr_temp_night, 'Trois-Rivières, nuit')
  
  legend_tr <- get_legend(tr_day)
  tr <- plot_grid(plot_grid(tr_day + theme(legend.position = 'none'),
                             tr_night + theme(legend.position = 'none'), align = 'h'),
                   legend_tr,
                   nrow = 1, rel_widths = c(6,1))
  
  ggsave('graphics/trtemp.png', tr, width = 12, height = 6, units = 'in', dpi = 450)
  
  return(list(mtl, tr))
  
}



calc_temp <- function(temp_dfs){

  full <- temp_dfs %>% 
    # take out first row that has units
    filter(Temperature != "°F") %>% 
    # format temperature and datetime correctly
    mutate(Temperature = str_replace(Temperature, ",", "."),
           Temperature = as.numeric(Temperature),
           tempC = (((Temperature-32)*5)/9),
           `FORMATTED DATE_TIME` = gsub("[.]", "\\1", `FORMATTED DATE_TIME`), 
           date_time = ymd_hms(`FORMATTED DATE_TIME`)) %>%
    # separate datetime into two columns
    separate(date_time, c("date", "time"), sep = " ") %>% 
    # create day/night variable
    mutate(time = hms(time),
           hour = hour(time),
           daytime = case_when(hour >= 5 & hour <= 20 ~ "day", 
                               .default = 'night')) %>% 
    # calculate mean temp per day for each sensor 
    group_by(plot_id, date, daytime) %>% 
    summarize(mean_temp = mean(tempC)) %>% 
    mutate(type = case_when(str_detect(plot_id, "CON") == T ~ 'Ruelle Traditionelle',
                            str_detect(plot_id, "RV") == T ~ 'Ruelle Verte'))
  
  con <- full %>% 
    filter(type == "Ruelle Traditionelle") %>% 
    group_by(date, daytime) %>% 
    summarize(plot_id = 'control group',
              type = 'Ruelle Traditionelle',
              mean_temp = mean(mean_temp))
  
  rv <- full %>% 
    filter(type == "Ruelle Verte")
  
  final <- rbind(con, rv)
  
  
}

plot_ind <- function(df, title){
  
  
  plot <- ggplot(df, aes(date, mean_temp, group = plot_id)) +
    geom_line(data = df[df$type == "Ruelle Verte",], aes(color = "Ruelle Verte"), alpha = 0.2) +
    geom_line(data = df[df$type == "Ruelle Traditionelle",], aes(color = "Moyenne Ruelle\nTraditionelle")) +
    scale_colour_manual(values = c("red", "grey20")) +
    labs(x = "", colour = "", y = "Température moyenne (\u00B0C)") +
    ggtitle(enquo(title)) + 
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    return(plot)
  
}
