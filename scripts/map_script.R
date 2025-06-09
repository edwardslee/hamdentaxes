library(tidyverse)
library(sf)
library(tidygeocoder)

town_properties <- st_read("data/townct_37800_0000_2010_s100_census_1_shp/nad83/")

df <- read_rds("data/all_streets_data_4_18_25_cleaned.rds")

ggplot(town_properties) +
  geom_sf() +
  theme_minimal()

hamden_shapefile <- town_properties |>
  filter(NAME10 == "Hamden")

hamden_shapefile |>
  ggplot() +
  geom_sf() +
  theme_minimal()


df_coords1 <- df[1:10000, ] |>
  mutate(full_address = str_to_title(address),
         town = "Hamden",
         state = "CT") |>
  geocode(street = full_address,
          city = town,
          state = state,
          method = "census",
          lat = latitude,
          long = longitude)
df_coords2 <- df[10001:20000, ] |>
  mutate(full_address = str_to_title(address),
         town = "Hamden",
         state = "CT") |>
  geocode(street = full_address,
          city = town,
          state = state,
          method = "census",
          lat = latitude,
          long = longitude)
df_coords3 <- df[20001:20221, ] |>
  mutate(full_address = str_to_title(address),
         town = "Hamden",
         state = "CT") |>
  geocode(street = full_address,
          city = town,
          state = state,
          method = "census",
          lat = latitude,
          long = longitude)
df_coords <- bind_rows(df_coords1, df_coords2, df_coords3)
df_old <- df_coords
df_coords <- df_old

df_coords <- df_coords |>
  mutate(tax_increase = property_tax_new - property_tax_old) |>
  filter(tax_increase >= 0,
         tax_increase < 11000)

df_coords_sf <- df_coords |>
  filter(complete.cases(df_coords)) |> st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

ggplot() +
  geom_sf(data = hamden_shapefile, fill = "white", color = "black") +
  geom_sf(data = df_coords_sf, aes(color = tax_increase), alpha = 0.5, size = 1) + 
  scale_color_viridis_c() + 
  theme_minimal() 


