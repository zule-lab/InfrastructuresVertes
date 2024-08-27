create_mtl_dags <- function(){
  
  canopy <- dagify(
    canopy_cover ~ landscape_structure,
    canopy_cover ~ vegetation_abundance,
    vegetation_abundance ~ funding,
    vegetation_abundance ~ management,
    vegetation_abundance ~ landscape_structure,
    funding ~ ruelle_type,
    ruelle_type ~ community_activity,
    ruelle_type ~ language, 
    ruelle_type ~ income, 
    ruelle_type ~ awareness,
    ruelle_type ~ resident_length,
    awareness ~ resident_length,
    awareness ~ language,
    values ~ language,
    values ~ income,
    values ~ ethnicity,
    income ~ ethnicity,
    income ~ education, 
    management ~ values, 
    management ~ income,
    labels = c(
      'canopy_cover' = 'Canopy Cover',
      'vegetation_abundance' = 'Vegetation Abundance',
      'landscape_structure' = 'Landscape Structure',
      'management' = 'Management',
      'funding' = 'Funding',
      'community_activity' = 'Community Activity',
      'ruelle_type' = 'Ruelle Type',
      'language' = 'Language',
      'income' = 'Income',
      'awareness' = 'Awareness of \nProgram',
      'resident_length' = 'Length of \nTime as Resident',
      'values' = 'Values wrt \nNature',
      'ethnicity' = 'Ethnicity',
      'education' = 'Education'),
    exposure = 'ruelle_type',
    outcome = 'canopy_cover') %>% 
    tidy_dagitty() %>%
    mutate(status = case_when(name == "tree_abundance" ~ 'outcome',
                              name == "ruelle_type" ~ 'exposure',
                              name == "income" ~ 'exposure',
                              name == "language" ~ 'exposure',
                              .default = 'NA'))
  
  
  
  tree_abund <- dagify(
    tree_abundance ~ landscape_structure,
    tree_abundance ~ management,
    tree_abundance ~ funding,
    funding ~ ruelle_type,
    ruelle_type ~ community_activity,
    ruelle_type ~ language, 
    ruelle_type ~ income, 
    ruelle_type ~ awareness,
    ruelle_type ~ resident_length,
    awareness ~ resident_length,
    awareness ~ language,
    values ~ language,
    values ~ income,
    values ~ ethnicity,
    income ~ ethnicity,
    income ~ education, 
    management ~ values, 
    management ~ income,
    labels = c(
      'tree_abundance' = 'Tree Abundance',
      'landscape_structure' = 'Landscape Structure',
      'management' = 'Management',
      'funding' = 'Funding',
      'community_activity' = 'Community Activity',
      'ruelle_type' = 'Ruelle Type',
      'language' = 'Language',
      'income' = 'Income',
      'awareness' = 'Awareness of \nProgram',
      'resident_length' = 'Length of \nTime as Resident',
      'values' = 'Values wrt \nNature',
      'ethnicity' = 'Ethnicity',
      'education' = 'Education'),
    exposure = 'ruelle_type',
    outcome = 'tree_abundance') %>% 
    tidy_dagitty() %>%
    mutate(status = case_when(name == "tree_abundance" ~ 'outcome',
                              name == "ruelle_type" ~ 'exposure',
                              name == "income" ~ 'exposure',
                              name == "language" ~ 'exposure',
                              .default = 'NA'))
  
  
  dag_canopy <- plot_dag(canopy)
  dag_firefly <- plot_dag(firefly)
  dag_tree_abund <- plot_dag(tree_abund)
  dag_tree_div <- plot_dag(tree_div)
  dag_veg_complex <- plot_dag(veg_complex)
  dag_imperv <- plot_dag(imperv)
  dag_inv <- plot_dag(inv)
  dag_cooling <- plot_dag(cooling)
  dag_tree_size <- plot_dag(tree_size)
  dag_flower <- plot_dag(flower)
  
  p <- (dag_canopy + dag_firefly + dag_tree_abund + dag_tree_div + dag_veg_complex) /
    (dag_imperv + dag_inv + dag_cooling + dag_tree_size + dag_flower)
  
  ggsave('graphics/mtl_dags.png', plot = i, width = 15, height = 12, units = "in")
  
}


shorten_dag_arrows <- function(tidy_dag, proportion){
  # Update underlying ggdag object
  tidy_dag$data <- dplyr::mutate(tidy_dag$data, 
                                 xend = (1-proportion/2)*(xend - x) + x, 
                                 yend = (1-proportion/2)*(yend - y) + y,
                                 xstart = (1-proportion/2)*(x - xend) + xend,
                                 ystart = (1-proportion/2)*(y-yend) + yend)
  return(tidy_dag)
}


plot_dag <- function(tidy_dag){
  
  dagified <- shorten_dag_arrows(tidy_dag, proportion = 0.2)
  
  
  i <- ggplot(dagified, aes(x = x, y = y, xend = xend, yend = yend)) +
    theme_dag() + 
    geom_dag_point(aes(color = status), size = 25) +
    geom_dag_label_repel(aes(label = label, fill = status),
                         color = "white", fontface = "bold", size = 6, nudge_x = -1) +
    geom_dag_edges(aes(x = xstart, y = ystart), edge_width = 1.5) + 
    scale_fill_manual(values = c('darkseagreen', 'grey', 'lightskyblue')) + 
    scale_colour_manual(values = c('darkseagreen', 'grey', 'lightskyblue')) + 
    theme(legend.position = 'none')
  
  return(i)
  
}
