
world_past_emissions <- function(start_year, carbon_df){
  carbon_df %>%
    filter(year <= start_year, entity == "World") %>%
    summarize(sum(annual_co2, na.rm = T))
}


q_luc_time_function <- function(year, start_year, end_year = 2100, luc_df){
  q_luc_start <- luc_df %>% 
    filter(year == start_year, entity == "World") %>%
    pull(annual_co2)
  
  q_luc_t <- q_luc_start * (1 - (year - start_year) / (end_year - start_year))
  
  return(q_luc_t)
}

q_future_luc_function <- function(start_year, end_year = 2100, luc_df){
  sum(sapply((start_year + 1):end_year, function(year) q_luc_time_function(year, start_year, end_year, luc_df)))
}

# Q_past(FFI) = world_past_emissions(start_year = 2012, ffi_df )
# 
# Q_past(LUC) = world_past_emissions(start_year = 2012, luc_df)
# 
# Q_future(LUC) = q_luc_future(start_year = 2012, luc_df)


q_ffi_future_function <- function(start_year, q_tot, ffi_df, luc_df, end_year = 2100){
  
  q_past_ffi <- world_past_emissions(start_year = start_year, end_year = end_year, 
                                     carbon_df = ffi_df)
  
  q_past_luc <- world_past_emissions(start_year = start_year, end_year = end_year,
                                     carbon_df = luc_df)
  
  q_future_luc <- q_future_luc_function(start_year = start_year, end_year = end_year,
                                        luc_df = luc_df) 
  
  q_ffi_future <- q_tot - q_past_ffi - q_past_luc - q_future_luc
  
  return(q_ffi_future)
}
