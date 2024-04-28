
library(tidyverse)
library(readxl)

carbon_ffi_df <- read_csv("/Users/ayw1327/Documents/GitHub/emissions-analysis/Data/annual-co2-emissions-per-country.csv") %>%
  rename(annual_co2 = `Annual CO₂ emissions`)
carbon_luc_df <- read_csv("~/Documents/GitHub/emissions-analysis/Data/co2-land-use.csv") %>%
  rename(annual_co2 = `Annual CO₂ emissions from land-use change`)


# Convert column names to lowercase and replace spaces with underscores
colnames(carbon_ffi_df) <- colnames(carbon_ffi_df) %>%
  tolower() 

colnames(carbon_luc_df) <- colnames(carbon_luc_df) %>%
  tolower() 

ggplot(data = ffi_df %>% filter(entity == "United States")) +
  geom_smooth(aes(x = year, y = annual_co2), method = "gam", color = "blue")
