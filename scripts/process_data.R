library(tidyverse)

df_raw <- read_rds("data/all_streets_data_4_19_25.rds")

mill_rate_new <- 46.61
mill_rate_old <- 55.61

df <- df_raw |>
  mutate(assessment_new = appraisal * 0.7,
         assessment_old = hist_total_value * 0.7,
         property_tax_new = assessment_new * mill_rate_new / 1000 |> round(),
         property_tax_old = assessment_old * mill_rate_old / 1000 |> round())

df <- df |>
  select(address, property_tax_new, property_tax_old, appraisal, assessment_new, hist_total_value, assessment_old) |>
  rename(appraisal_new = appraisal,
         appraisal_old = hist_total_value)

write_rds(df, file = "data/all_streets_data_4_19_25_cleaned.rds")
