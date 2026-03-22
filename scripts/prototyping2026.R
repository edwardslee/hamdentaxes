library(tidyverse)

df_new <- read_rds("data/all_streets_data_02-17-26.rds")
df_old <- read_rds("data/all_streets_data_4_19_25_cleaned.rds")

df_new 

df_new_appr <- df_new |>
  select(address, appraisal) |>
  mutate(address = str_trim(address))

df_old <- df_old |>
  mutate(address = str_trim(address))

df <- left_join(df_old, df_new, by = "address", suffix = c(".2025", ".2026"))



df_old_relab <- df_old |> 
  distinct() |> 
  select(address, appraisal_new, appraisal_old) %>%
  group_by(address) |>
  arrange(appraisal_new, .by_group = TRUE) |>
  mutate(
    house_number = as.numeric(str_extract(address, "^[0-9]+")),
    street_name  = str_remove(address, "^[0-9]+ ")
  ) |>
  arrange(street_name, house_number) |>
  select(-house_number, -street_name) |>
  mutate(address = if (n() > 1) paste0(address, " ", LETTERS[seq_len(n())]) else address) |>
  ungroup()

df_new_relab <- df_new_appr |> 
  distinct() |> 
  select(address, appraisal) |>
  group_by(address) |>
  arrange(appraisal, .by_group = TRUE) |>
  mutate(
    house_number = as.numeric(str_extract(address, "^[0-9]+")),
    street_name  = str_remove(address, "^[0-9]+ ")
  ) |>
  arrange(street_name, house_number) |>
  select(-house_number, -street_name) |>
  mutate(address = if (n() > 1) paste0(address, " ", LETTERS[seq_len(n())]) else address) |>
  ungroup()


df <- left_join(df_old_relab, df_new_relab, by = "address", suffix = c(".2025", ".2026"))

# checking how many have different appraisals after appeals
df |>
  mutate(difference = appraisal - appraisal_new) |>
  filter(difference != 0)


# saving out updated data frame
df_out <- df |>
  select(address, appraisal, appraisal_old) |>
  mutate(assessment_new = 0.7 * appraisal,
         assessment_old = 0.7 * appraisal_old,
         appraisal_year1 = appraisal_old + 0.25 * (appraisal - appraisal_old),
         assessment_year1 = 0.7 * appraisal_year1, 
         appraisal_year2 = appraisal_old + 0.5 * (appraisal - appraisal_old),
         assessment_year2 = 0.7 * appraisal_year2) 

write_csv(df_out, file = "data/02-17-26_allstreets.csv")
write_rds(df_out, file = "data/02-17-26_allstreets.rds")
