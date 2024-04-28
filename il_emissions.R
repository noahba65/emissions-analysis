library(tidyverse)
library(readxl)

emissions <- read_xlsx("/Users/ayw1327/Documents/GitHub/emissions-analysis/Data/AllStateGHGDataPY2023_100323.xlsx",
                       sheet = "Data by UNFCCC-IPCC Sectors")


il_emissions <- emissions %>%
  filter(STATE == "IL") %>%
  pivot_longer(
    cols = starts_with("Y"),   # Selects columns that start with 'Y'
    names_to = "Year",         # New column for the years
    names_prefix = "Y",        # Removes the 'Y' prefix from year columns
    values_to = "Emissions"        # New column for the values
  ) %>%
  mutate(Year = as.numeric(Year))

# Emissions by sector
il_emissions %>% 
  filter(Year == 2021) %>%
  group_by(SECTOR) %>%
  summarize(total = sum(Emissions, na.rm = T)) %>%
  arrange(desc(total))

il_carbon_ts <- il_emissions %>%
  filter(GHG == "CO2") %>%
  group_by(Year) %>%
  summarize(yearly_total = sum(Emissions, na.rm = T)) 

 
ggplot(data = il_carbon_ts) +
  geom_point(aes(x = Year, y = yearly_total, group = 1)) +
  geom_smooth(aes(x = Year, y = yearly_total, group = 1), method = "gam", color = "blue") + # Fitting a linear model
  
  ylim(0, 240) +
  scale_x_continuous(breaks = seq(min(il_carbon_ts$Year), max(il_carbon_ts$Year), by = 10)) +
  ggtitle("Total Emissions")

####### Fitting model ##########
model <- loess(yearly_total ~ Year, data = il_carbon_ts, span = 0.75)  # Adjust the span as necessary for smoothing


# Create a new data frame for future years (extending 50 years into the future)
future_years <- data.frame(Year = seq(max(il_carbon_ts$Year) + 1, max(il_carbon_ts$Year) + 50))

# Predict future values using the loess model
future_years$predicted_total = predict(model, newdata = future_years)

# Combine the existing and future data
combined_data <- bind_rows(il_carbon_ts, future_years)

# Plotting original data points and the predicted trend
ggplot() +
  geom_point(aes(x = Year, y = yearly_total), data = il_carbon_ts, colour = "blue") +  # Original data points
  geom_line(aes(x = Year, y = predicted_total), data = combined_data, colour = "red") +  # Predicted trend line
  ylim(0, 240) +
  scale_x_continuous(breaks = seq(min(il_carbon_ts$Year), max(future_years$Year), by = 10)) +
  ggtitle("Total Emissions Forecast")



  