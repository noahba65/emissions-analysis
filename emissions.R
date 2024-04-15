library(tidyverse)
library(readxl)

emissions <- read_xlsx("/Users/noahanderson/Downloads/allstateghgdatapy2023readme_100323_0/AllStateGHGDataPY2023_100323.xlsx",
                       sheet = "Data by UNFCCC-IPCC Sectors")


il_emissions <- emissions %>%
  filter(STATE == "IL") %>%
  pivot_longer(
    cols = starts_with("Y"),   # Selects columns that start with 'Y'
    names_to = "Year",         # New column for the years
    names_prefix = "Y",        # Removes the 'Y' prefix from year columns
    values_to = "Emissions"        # New column for the values
  )

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
  geom_point(aes(x = Year, y = yearly_total)) +
  ggtitle("Total Emissions")



  