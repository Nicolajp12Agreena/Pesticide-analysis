# Import necessary libraries
library(tidyverse)

# Set working directory
setwd("C:/Users/nicolaj.petersen/OneDrive - Agreena/Skrivebord/R_scripts/work/cqa-pesticide-values/data")
list.files(getwd())

# --------------------------------------------
# Calculate Share of Land Being Permanent Crops
# --------------------------------------------

# Read land area data for arable and permanent crops
df_area <- read.csv("FAOSTAT_land_area_arable_permanent.csv")

# Filter data for the year 2022
df_area <- df_area %>% 
  filter(Year == 2022)

# Calculate the share (%) of permanent crops for each country
df_share_area <- df_area %>% 
  group_by(Area) %>% 
  mutate(share_permanent_crops = Value / sum(Value) * 100) %>% 
  filter(Item == 'Permanent crops')

# Create a bar chart showing the share of permanent crops
ggplot(df_share_area, aes(x = reorder(Area, share_permanent_crops), y = share_permanent_crops)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = c(2.5, 5), linetype = "dashed", color = "red", size = 1) +
  coord_flip() +
  labs(
    title = 'Share of Cropland Area Having Permanent Crops',
    subtitle = 'Time period: 2022 \nRed dashed lines at 2.5% and 5%',
    caption = 'Based on data from FAOSTAT\nhttps://www.fao.org/faostat/en/#data/RL',
    x = 'Country',
    y = '% of land having permanent crops'
  ) +
  theme_minimal()

# Note: Assuming higher amount on cropland compared to permanent cropland, we might underestimate a bit.

# ----------------------------------------------------------
# Calculate Mean Application of Pesticides for Countries in 2022
# ----------------------------------------------------------

# Read pesticide usage data
df_pesticides <- read.csv("FAOSTAT_pesticides.csv")

# Filter data and reshape to wide format
df_pest_share <- df_pesticides %>%
  filter(Element != "Use per area of cropland", Year == 2022) %>% 
  select(Area, Item, Value) %>% 
  pivot_wider(names_from = Item, values_from = Value) %>%  
  select(Area, Insecticides, Herbicides, `Fungicides and Bactericides`)

# Read arable land data
df_area_arable <- read.csv("land_arable.csv") %>%
  filter(Year == 2022)

# Calculate total area of temporary crops and temporary fallow (in hectares)
df_temp_crop_fallow <- df_area_arable %>% 
  select(Area, Item, Value) %>% 
  filter(Item %in% c("Temporary crops", "Temporary fallow")) %>%
  group_by(Area) %>% 
  summarise(temporary_crop_and_fallow_hectares = sum(Value) * 1000)  # Convert to hectares

# Get arable land area (in hectares)
df_arable_land <- df_area %>% 
  filter(Item == "Arable land") %>%
  select(Area, arable_land_value = Value)

# Rename columns in df_share_area for clarity
df_share_area <- df_share_area %>% 
  select(Area, permanent_crop_value = Value, share_permanent_crops)

# Merge data frames and calculate pesticide application rates
df_calc <- df_temp_crop_fallow %>% 
  inner_join(df_pest_share, by = "Area") %>% 
  inner_join(df_share_area, by = "Area") %>% 
  inner_join(df_arable_land, by = "Area") %>% 
  mutate(
    # Convert pesticide usage from tonnes to kilograms
    kg_Insecticides = Insecticides * 1000,
    kg_Herbicides = Herbicides * 1000,
    kg_Fungicides = `Fungicides and Bactericides` * 1000,
    # Calculate pesticide application rates (kg per hectare)
    kg_ha_insecticide = kg_Insecticides / temporary_crop_and_fallow_hectares,
    kg_ha_herbicide = kg_Herbicides / temporary_crop_and_fallow_hectares,
    kg_ha_fungicide = kg_Fungicides / temporary_crop_and_fallow_hectares,
    kg_ha_pesticide = kg_ha_insecticide + kg_ha_herbicide + kg_ha_fungicide,
    # Calculate total arable and permanent crop areas in hectares
    arable_ha = arable_land_value * 1000,
    permanent_ha = permanent_crop_value * 1000,
    # Calculate the share of meadows and pastures
    meadows_and_pastures_share = (1 - temporary_crop_and_fallow_hectares / arable_ha) * 100
  ) 
  # Filter countries with less than 5% permanent crops and less than 10% meadows and pastures
write.csv(df_calc, file = 'countries pesticides FAO calculation.csv')
# The final dataset 'df_calc' now contains the calculated pesticide application rates and land areas.
