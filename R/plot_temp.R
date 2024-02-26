plot_temp <- function(temp_dfs, tr_temp_dfs) {
  
  temp <- temp_dfs %>% 
    filter(date > '2023-01-01') %>%
    filter(Temperature != "°F") %>% 
    mutate(Temperature = str_replace(Temperature, ",", "."),
           Temperature = as.numeric(Temperature),
           tempC = (((Temperature-32)*5)/9),
           `FORMATTED DATE_TIME` = gsub("[.]", "\\1", `FORMATTED DATE_TIME`), 
           date_time = ymd_hms(`FORMATTED DATE_TIME`)) %>%
           separate(date_time, c("date", "time"), sep = " ") %>% 
    mutate(daytime = case_when(time > "5:00:00" | time < "21:00:00" ~ 'day',
                               time <= "5:00:00" & time >= "21:00:00" ~ 'night',
                               .default = 'night')) %>% 
    drop_na(tempC)
  
  
}
