create_nat_fig <- function(pn_vsmpe_brms_sample, pn_tr_brms_sample, ecological_benefits){
  
 df <- ecological_benefits %>%
    mutate(type = case_when(type == "Ruelles Traditionelles" ~ "Grey Alleys",
                            type == "Ruelles Vertes" ~ "Green Alleys", 
                            type == "Segments des Rues" ~ "Street Segments"),
           prop_nat = replace_na(number_trees_nat/total_trees, 0))
  
  plot <- df %>% 
    ggplot() + 
    geom_boxplot(aes(x = type, y= prop_nat), colour = "grey50", alpha = 0.2, outliers = F) +
    geom_jitter(aes(x = type, y = prop_nat), height = 0, width = 0.3, colour = 'red', data = df %>% filter(total_trees == 0)) + 
    geom_jitter(aes(x = type, y= prop_nat), data = df %>% filter(total_trees != 0)) + 
    labs(y = "Proportion of Native Trees", x = "") + 
    theme_classic() + 
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(colour = 'black', size = 12), 
          strip.text = element_text(colour = 'black', size = 12)) + 
    facet_wrap(~city)
  
  ggsave('graphics/nativefig.png', plot, width = 10, height = 6, units = 'in')
  
}