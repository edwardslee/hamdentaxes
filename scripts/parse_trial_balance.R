library(tidyverse)
library(stringr)
library(pdftools)


file <- "data/Detail TB 6.30.24.pdf"

txt <- pdf_text(file)

parse_lines <- function(tx) {
  tx <- str_trim(tx, side = "left")
  tx <- str_replace_all(tx, pattern = "'", "")
  tx <- str_replace_all(tx, pattern = '"', "")
  tx <- str_replace_all(tx, pattern = "&", "AND")
  string_len <- str_length(tx)
  # if (string_len < 100 |
  #     grepl("R VENDOR & PREC.|BKGRND CHKS|MOVING VIOLATIONS|R CERTIFICATE OF|SIDEWALK|SCHOLL BUS|PILOT - GREATER|SEMINARS|TRANSFER STATION|RENTALSEVICES|CASH-GENERAL|GF VIEW POINT|CASH-BOARD OF|CASH BOND PROCEEDS|CASH TAX COLLECTOR", tx)) {
  first_word <- str_extract(tx, "^[^ ]+")
  has_num  <- str_detect(first_word, "\\d")
  has_char <- str_detect(first_word, "[A-Za-z]")
  if (first_word == "CAPITAL") {
    new_string <- "CAPITAL PROJECTS,,,,,23204886.03,13118528.95,19998942.56,-6880413.61,16324472.42\n"
    return(new_string)
  } else if (first_word == "MEDICAL") {
    new_string <-"MEDICAL INSURANCE FUND-IS,,,,,3561767.24,306449086.59,306947997.01,-498910.42,3062856.82\n"
    return(new_string)
  } else if ((has_num & string_len < 29) | tx == "015 PARKS AND REC FIELD IMP-CP" | tx == "019 PENSION - EMPLOYEE RETIRE FUND"| tx == "032 MEDICAL INSURANCE FUND-IS" | tx == "034 OPEB - OTHER POST EMP BENEFIT" |
             tx == "047 LIBRARY ENDOWMENT FUND-TA" | tx == "059 PERFORMANCE BONDS FUND-TA" | tx == "069 DEFERRED COMPENSATION PLN" | tx == "077 GF - BROOKSVALE GIFT FUND-TA" | tx == "087 TR - TOWN CENTER/FARMERS MKT" | tx == "093 YS - SCHOOL AGE CHILD CARE" |
             tx == "094 YS - CHILD CARE FOOD PROG-SP" | tx == "0A3 SLEEPING GIANT DAY CARE-SP" | tx == "0D7 COMM. SERV. - EMER.FOOD AND SHEL"| tx == "0AC ARTS, CULTURAL AND REC PROGRAMS" |
             tx == "0B7 BARBARA ANN LYONS FUND-TA" | tx == "0BR BROOKSVALE PARK RENO/ACQ-CP" | tx == "0C7 JOHN J. LADEN MEMORIAL-TA" | tx == "0CO CENTER ONE GOVT CENTER-CP" |
             tx == "0CP HAMDEN COMMUNITY POL GIFT FUND" | tx == "0DE YS - BREUAU GRANT- STATE -EDUC" | tx == "GFA GEN FIXED ASSET ACCT GRP" | tx == "GFA -30-00-103-10041 -") {
    tx <- str_replace_all(tx, ",", "")
    new_string <- tx |> str_c(",,,,,,,,,\n")
    return(new_string)
  } else if (has_char) {
    tx <- str_replace_all(tx, ",", "")
    locations <- str_locate(tx, "^.*?(?=\\d)")
    end_dept <- locations[1, 2]
    str_sub(tx, start = end_dept, end = end_dept) <- ",,,"
    locations <- str_locate_all(tx, "(?:\\d{1,3}(?:,\\d{3})*|\\d*)\\.\\d{2}")[[1]]
    num <- locations[1, 1]
    str_sub(tx, start = num - 1, end = num - 1) <- ","
    tx <- str_c(tx, ",,,,,\n")
    return(tx)
  } else {
    tx <- str_trim(tx, side = "left")
    tx <- str_replace_all(tx, ",", "")
    first_word <- str_extract(tx, "^[^ ]+")
    has_hyphen <- str_detect(first_word, "-")
    
    if (!has_hyphen) {
      ind_date <- str_locate_all(tx, "\\d{2}/\\d{2}/\\d{2}")[[1]]
      ind_date_start <- ind_date[[1]]
      ind_date_end  <- ind_date[[2]]
      
      # Split the string into two parts
      part1 <- substr(tx, 1, ind_date_start - 1)
      part2 <- substr(tx, ind_date_start, nchar(tx))
      
      # Replace spaces with commas in the first part
      part1_replaced <- str_replace_all(part1, " ", ",")
      
      # Combine the parts back together
      result <- paste0(part1_replaced, part2)
      
      str_sub(result, start = ind_date_end + 1, end = ind_date_start - 1) <- ","
      locations <- str_locate_all(result, "-?(\\d*)\\.\\d{2}")[[1]]
      first_num <- locations[1, 1]
      second_num <- locations[2, 1]
      third_num <- locations[3, 1]
      str_sub(result, start = first_num - 1, end = first_num - 1) <- ","
      str_sub(result, start = second_num - 1, end = second_num - 1) <- ","
      str_sub(result, start = third_num - 1, end = third_num - 1) <- ","
      result <- str_c(result, ",\n")
      result <- str_c(str_sub(result, 1, first_num - 1), ",", str_sub(result, first_num))
      
      return(result)
    } else {
      result <- str_c(",", tx)
      locations <- str_locate_all(result, "-?(\\d*)\\.\\d{2}")[[1]]
      beg_bal <- locations[1, 1]
      debits <- locations[2, 1]
      credits <- locations[3, 1]
      netchange <- locations[4, 1]
      endbal <- locations[5, 1]
      str_sub(result, start = beg_bal - 1, end = beg_bal - 1) <- ","
      str_sub(result, start = debits - 1, end = debits - 1) <- ","
      str_sub(result, start = credits - 1, end = credits - 1) <- ","
      str_sub(result, start = netchange - 1, end = netchange - 1) <- ","
      str_sub(result, start = endbal - 1, end = endbal - 1) <- ","
      result <- str_c(result, "\n")
      result <- str_c(str_sub(result, 1, beg_bal - 1), ",,,", str_sub(result, beg_bal))
      
      return(result)
    }
  }
}

str <- c("PER,JNL,SRC,EFF_DATE,REFERENCE,BEG_BALANCE,DEBITS,CREDITS,NET_CHANGE,END_BALANCE\n")

# for (i in seq(txt)) {
for (i in 1:885) {
  print(i)
  # str <- c("PER,JNL,SRC,EFF_DATE,REFERENCE,BEG_BALANCE,DEBITS,CREDITS,NET_CHANGE,END_BALANCE\n")
  test <- txt[[i]]
  
  # remove column labels and other crap
  header <- test |> str_locate("REFERENCE")
  header_end <- header[2] + 2
  test_tr <- str_sub(test, start = header_end) 
  
  # remove text below tables
  footer <- test_tr |> str_locate("Report generated")
  footer_start <- footer[1] - 1
  test_tr <- test_tr |> str_sub(end = footer_start)
  
  # collapse all spaces to one space and all new lines to one new line
  test_tr <- test_tr |> str_replace_all(" +", " ")
  test_tr <- test_tr |> str_replace_all("\n+", "\n")
  
  
  # loc_newline <- test_tr |> str_locate_all("\n")
  # test_len <- loc_newline[[1]][, 1, drop = FALSE] |>
  #   as_tibble() |>
  #   mutate(len = start - lag(start) - 1)
  # test_len[1, "len"] <- test_len[1, "start"] - 1
  
  df <- tibble(text = test_tr)
  df <- df |>
    mutate(lines = str_split(text, "\\n"))
  df_lines <- df |>
    unnest(lines) |> 
    select(lines)
  
  df_lines <- df_lines |>
    mutate(len = str_length(lines)) |>
    filter(!grepl(pattern = " TOTALS FOR FUND", lines))
  
  df_lines <- df_lines |>
    filter(len > 0) |>
    mutate(csv_form = map_chr(lines, parse_lines))
  
  
  x <- str_c(df_lines$csv_form, collapse = "")
  
  str <- str_c(str, x)
  df <- read.table(text = str, sep =",", header = TRUE, stringsAsFactors = FALSE)
  df
  print(nrow(df))
  warnings()
}

