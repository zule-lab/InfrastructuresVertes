select_insects <- function(study_rv, study_controls){
  
  all <- rbind(study_rv, study_controls)
  
  insects <- all %>%
    filter(RUELLE_CODE == "RV-VSMPE-2" | 
             RUELLE_CODE == "RV-VSMPE-3" | 
             RUELLE_CODE == "RV-VSMPE-6" | 
             RUELLE_CODE == "RV-VSMPE-8" | 
             RUELLE_CODE == "RV-VSMPE-10" | 
             RUELLE_CODE == "RV-VSMPE-30" | 
             RUELLE_CODE == "RV-VSMPE-22" | 
             RUELLE_CODE == "RV-VSMPE-39" | 
             RUELLE_CODE == "RV-VSMPE-29" |
             RUELLE_CODE == "RV-VSMPE-35" |
             RUELLE_CODE == "RV-VSMPE-26" |
             RUELLE_CODE == "RV-VSMPE-31" |
             RUELLE_CODE == "RV-VSMPE-36" |
             RUELLE_CODE == "RV-VSMPE-25" |
             RUELLE_CODE == "RV-VSMPE-23" |
             RUELLE_CODE == "RV-VSMPE-32" |
             RUELLE_CODE == "RV-VSMPE-40" |
             RUELLE_CODE == "RV-VSMPE-13" |
             RUELLE_CODE == "RV-VSMPE-16" |
             RUELLE_CODE == "RV-VSMPE-17" |
             RUELLE_CODE == "RV-VSMPE-20" |
             RUELLE_CODE == "RV-VSMPE-21" |
             RUELLE_CODE == "CON-VSMPE-1" |
             RUELLE_CODE == "CON-VSMPE-7" |
             RUELLE_CODE == "CON-VSMPE-8" |
             RUELLE_CODE == "CON-VSMPE-9" |
             RUELLE_CODE == "CON-VSMPE-4" )
  
  return(insects)
}