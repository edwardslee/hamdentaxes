library(tidyverse)
library(pdftools)
library(tabulapdf)

f <- "data/2024 State Street List.pdf"

# df <- extract_tables(f)
df <- pdf_ocr_text(pdf = f)

polling_places <- c(
  "Bear Path School",
  "Board Of Education Building",
  "Booker T Washington Academy",
  "Dunbar Hill School",
  "Keefe Community Center",
  "Miller Library",
  "Ridge Hill School",
  "Spring Glen School",
  "West Woods School"
                    )

test_string <- df[1]

rows <- str_split(test_string, "\n")[[1]]
rows <- rows[rows != ""]

# Process the header row
header <- rows[1]
header <- sub("STREET NAME ", "STREET NAME,", header)
header <- sub("POLLING PLACE ", "POLLING PLACE,", header)

# Build a regex pattern that matches a space before or after any polling place
polling_pattern <- paste(polling_places, collapse = "|")

# Process all other rows
data_rows <- sapply(rows[-1], function(row) {
  # Replace " <polling place>" with ",<polling place>"
  row <- str_replace(row, paste0(" (", polling_pattern, ")"), ",\\1")
  # Replace "<polling place> " with "<polling place>,"
  row <- str_replace(row, paste0("(", polling_pattern, ") "), "\\1,")
  return(row)
})

# Combine header and data rows
csv_lines <- c(header, data_rows)
csv_string <- paste(csv_lines, collapse = "\n")
csv_string <- str_c(csv_lines, "\n")
cat(csv_string)

# do this for every other page without the header row and concantenate
for (i in 2:20) {
  test_string <- df[i]
  rows <- str_split(test_string, "\n")[[1]]
  rows <- rows[rows != ""]
  rows <- rows[-1]
  
  # Process all other rows
  data_rows <- sapply(rows, function(row) {
    # Replace " <polling place>" with ",<polling place>"
    row <- str_replace(row, paste0(" (", polling_pattern, ")"), ",\\1")
    # Replace "<polling place> " with "<polling place>,"
    row <- str_replace(row, paste0("(", polling_pattern, ") "), "\\1,")
    return(row)
  })
  
  # Combine header and data rows
  csv_lines <- c(data_rows)
  csv_string2 <- paste(csv_lines, collapse = "\n")
  csv_string2 <- str_c(csv_string2, "\n")
  csv_string <- str_c(csv_string, csv_string2)
}

writeLines(csv_string, "data/street_districts.csv")

df_district <- read_csv("data/street_districts.csv", col_types = "ccc")
names(df_district) <- c("address", "polling_place", "range")

df_district <- df_district |>
  separate(range, into = c("house_num_low", "house_num_high"), sep = "-")

polling_to_district <- tibble(
  polling_place = c(
    "Bear Path School",
    "Board Of Education Building",
    "Booker T Washington Academy",
    "Dunbar Hill School",
    "Keefe Community Center",
    "Miller Library",
    "Ridge Hill School",
    "Spring Glen School",
    "West Woods School"
  ),
  district = c(8, 5, 2, 7, 3, 1, 6, 4, 9)
)

df_district <- df_district |>
  left_join(polling_to_district, by = c("polling_place"))

write_csv(df_district, file = "data/address_district.csv")
write_rds(df_district, file = "data/address_district.rds")
