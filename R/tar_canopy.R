tar_canopy <- function(){
  
  c(
    
    tar_group_by(
      rv_by_ruelle,
      rv,
      RUELLE_ID
    ),
    
    tar_group_by(
      controls_by_ruelle,
      controls,
      RUELLE_ID
    ),
    
    tar_target(
      can_cov_rv,
      calc_can(rv_by_ruelle, canopy_path),
      map(rv_by_ruelle),
      iteration = 'list'
    ),
    
    tar_target(
      can_cov_controls,
      calc_can(controls_by_ruelle, canopy_path),
      map(controls_by_ruelle),
      iteration = 'list'
    ),
    
    tar_target(
      can_cov_rv_bind,
      do.call(rbind, can_cov_rv)
    ),
    
    tar_target(
      can_cov_controls_bind,
      do.call(rbind, can_cov_controls)
    ),
    
    tar_target(
      can_cov_street,
      calc_can_street(street_segments_rv, street_segments_control, sidewalks, tr_sidewalks, canopy_path)
    )
    
  )
  
  
}