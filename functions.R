
world_past_emissions<- function(start_year, carbon_df){
  carbon_df %>%
    filter(year < start_year, entity == "World") %>%
    summarize(sum(annual_co2, na.rm = T))
}

# Q (t-T0)=-Q0(LUC)2100 - T0(t - T0)+ Q0(LUC)

projected_q_luc <- function(year, q_luc_start, ){
  
}

q_luc_time_function <- function(year, start_year, luc_df){
  q_luc_start <- luc_df %>% 
    filter(year == start_year) %>%
    pull(annual_co2)
  
  end_year <- 2100
  
  
  
}