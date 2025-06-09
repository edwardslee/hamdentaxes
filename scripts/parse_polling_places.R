library(pdftools)
library(tabulapdf)
library(tidyverse)

f <- "data/2024 State Street List for Hamden as of 2_8_2024 (1).pdf"

df <- extract_tables(f)


df_all <- tibble(street = character(), polling_place = character())
for (i in seq(df)) {
  df_temp <- df[[i]]
  df_temp <- df_temp[, 1:2]
  names(df_temp) <- c("street", "polling_place")
  
  df_all <- bind_rows(df_all, df_temp)
}

df <- df_all |> filter(complete.cases(street)) |> distinct(street, polling_place) 

write_rds(df, file = "address_polling_places.rds")
