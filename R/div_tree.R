div_tree <- function(trees_clean){
  
  trees_long <- trees_clean %>% 
    mutate(spcode = toupper(paste0(str_sub(Genus, 1, 2), "", str_sub(Species, 1, 2))),
           spcode = case_when(scientific_name == "Acer saccharum" ~ "ACSAC",
                              .default = spcode)) %>%
    select(c(InfrastructureID, spcode)) %>% 
    group_by(InfrastructureID, spcode) %>% 
    tally()
  
  trees_w <- trees_long %>% 
    pivot_wider(id_cols = InfrastructureID, names_from = spcode, values_from = n, values_fill = 0 )
  
  # estimate abundance
  trees_abund <- trees_clean %>% 
    group_by(InfrastructureID) %>% 
    summarize(abund = n())
  
  # estimate diversity
  mat <- trees_w %>% 
    column_to_rownames("InfrastructureID")
  # Shannon diversity
  shan <- as.data.frame(diversity(mat, "shannon"))
  # Pielou's evenness
  even <- as.data.frame(diversity(mat, "shannon")/(log(specnumber(mat))))
  # Simpson diversity 
  simp <- as.data.frame(diversity(mat, "simpson"))
  # Number of species 
  spec <- as.data.frame(specnumber(mat))
  
  # join together
  div <- cbind(shan, even, simp, spec)
  div <- div %>%
    rename(Shannon = `diversity(mat, "shannon")`, 
           Even = `diversity(mat, "shannon")/(log(specnumber(mat)))`, 
           Simpson = `diversity(mat, "simpson")`,
           SpeciesRichness = `specnumber(mat)`) %>% 
    rownames_to_column("InfrastructureID") %>% 
    inner_join(., trees_abund, by = "InfrastructureID")
  
}