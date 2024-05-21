plot_temp <- function(temp_mit) {
  
  
  cooling_per_day <- temp_mit %>% 
    mutate(type = case_when(str_detect(plot_id, "CON") == T ~ "Ruelle Traditionelle",
                            str_detect(plot_id, "RV") == T ~ "Ruelle Verte"),
           date = date(date_time)) %>% 
    group_by(plot_id, date, tod) %>% 
    summarize(type = first(type), 
              CODE_ARR = first(CODE_ARR), 
              mean_cooling = mean(cooling, na.rm = T))
  
  day_mtl <- cooling_per_day %>% 
    filter(CODE_ARR == "VSMPE", tod == "day" & type == "Ruelle Verte")
  
  night_mtl <- cooling_per_day %>% 
    filter(CODE_ARR == "VSMPE", tod == "night" & type == "Ruelle Verte")
  
  day_tr <- cooling_per_day %>% 
    filter(CODE_ARR == "TR", tod == "day" & type == "Ruelle Verte")
  
  night_tr <- cooling_per_day %>% 
    filter(CODE_ARR == "TR", tod == "night" & type == "Ruelle Verte")
  
  
  ggplot(day_tr, aes(date, mean_cooling, group = plot_id)) +
    geom_line(alpha = 0.5, color = "grey20") +
    labs(x = "", colour = "", y = "Effet de refroidissement (\u00B0C)") +
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
  
  
}
