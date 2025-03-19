library(tidyverse)

df_raw <- read_rds("data/all_streets_data_4_18_25.rds")

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

write_rds(df, file = "data/all_streets_data_4_18_25_cleaned.rds")

example_st <- "32 LANSDOWNE AVE"

df_home <- df |>
  filter(address == example_st)

prop_tax_new <- df_home[["property_tax_new"]] |> round()
prop_tax_old <- df_home[["property_tax_old"]] |> round()
appraisal_new <- df_home[["appraisal_new"]] |> round()  
appraisal_old <- df_home[["appraisal_old"]] |> round()  
prop_tax_diff <- prop_tax_new - prop_tax_old 
prop_perc_inc <- formatC(prop_tax_diff / prop_tax_old * 100, digits = 1, format = "f")
monthly_payment <- prop_tax_diff / 12 |> round()

# adding commas within the numbers for easier reading
prop_tax_new <- prop_tax_new |> formatC(format="d", big.mark=",")
prop_tax_old <- prop_tax_old |> formatC(format="d", big.mark=",")
appraisal_new <- appraisal_new |> formatC(format="d", big.mark=",")
appraisal_old <- appraisal_old |> formatC(format="d", big.mark=",")
monthly_payment <- formatC(monthly_payment, digits = 2, format = "f")

string1 <- str_c("With the proposed mill rate of 55.61, your property taxes will be $",
                 prop_tax_new,
                 " based on the current value of your home of $",
                 appraisal_new, ".")
string2 <- str_c("In 2024, your property taxes was $",
                 prop_tax_old,
                 ", and your home was valued at $",
                 appraisal_old, ".")
string3 <- str_c("Your property taxes will increase by $",
                 prop_tax_diff,
                 ", which is a ",
                 prop_perc_inc, 
                 "% increase, and you will pay an extra $",
                 monthly_payment,
                 " per month in taxes.")

df_home <- df_home |>
  rename(`Proposed New Property Tax` = property_tax_new,
         `2024 Property Tax` = property_tax_old,
         `Appraisal - 2025` = appraisal_new,
         `Appraisal - 2024` = appraisal_old,
         `Tax Assessment - 2025` = assessment_new,
         `Tax Assessment - 2024` = assessment_old)

# finding nice y max limits for each number
ymax_tax <- df_home$`Proposed New Property Tax`
ymax_tax <- ceiling(ymax_tax / 1000) * 1000 # round to nearest thousand that's larger
ymax_tax <- ifelse(ymax_tax %% 2 != 0, ymax_tax + 2000, ymax_tax + 1000)

ymax_appraisal <- df_home$`Appraisal - 2025`
ymax_appraisal <- ceiling(ymax_appraisal / 25000) * 25000 + 10000

# plot for 2024 and new proposed property taxes
df_home |>
  pivot_longer(cols = -address) |>
  filter(grepl("Property", name)) |>
  mutate(group = as_factor(1:2)) |>
  ggplot() +
  geom_col(aes(name, value, fill = group), width = 0.65) +
  xlab("") +
  ylab("Property Tax") +
  scale_fill_manual(values = c("#fc4b11", "#b1eb61")) +
  scale_y_continuous(breaks = seq(0, ymax_tax, by = 2000)) + 
  theme_classic() +
  theme(legend.position="none")

# plot for 2024 and new appraisal  
df_home |>
  pivot_longer(cols = -address) |>
  filter(grepl("Appraisal", name)) |>
  mutate(group = as_factor(1:2)) |>
  ggplot() +
  geom_col(aes(name, value, fill = group), width = 0.65) +
  xlab("") +
  ylab("Appraisal Tax") +
  scale_fill_manual(values = c("#73b0d9", "#a9e1e6")) +
  scale_y_continuous(breaks = seq(0, ymax_appraisal, by = 50000)) + 
  theme_classic() +
  theme(legend.position="none")




email_string <- 
  str_c(
  "To Mayor Lauren Garret and the Hamden Legislative Council Members, ",
  "I am writing as a homeowner in Hamden, CT, to urge you to reconsider the proposed mill rate of 46.61 ",
  "and instead adopt a significantly lower rate—ideally in the mid-to-low 30s. This drastic increase in ",
  "property taxes would place an overwhelming financial burden on residents, many of whom are already struggling",
  "with rising costs in nearly every aspect of daily life, from exorbitant grocery prices to some of the highest utility rates in the nation. ",
  "For my property, the proposed mill rate would increase my property taxes by $", prop_tax_new, ", which is a ", prop_perc_inc, "% increase compared to my 2024 property taxes.\n\n",
  "Connecticut is already one of the most expensive states to live in, and Hamden ranks among its costliest towns. With the new property appraisals set to take effect in May 2025, many homeowners have seen their home values increase by 40% to 60%. Under the proposed mill rate, the average homeowner’s annual property tax bill would rise over 20%, adding hundreds of dollars to monthly expenses. This is not only an unsustainable financial strain on individual residents but also a significant threat to the town’s long-term economic health.\n\n",
  "A town thrives when it attracts and retains taxpaying homeowners who invest in and maintain their properties. Excessive property taxes will discourage new residents and businesses from moving to Hamden while forcing out long-time homeowners—especially retirees who may have paid off their mortgages but can no longer afford the tax burden. If this trend continues, Hamden risks declining property values, reduced homeownership, and an eroded sense of community.\n\n",
  "Homeowners cannot and should not bear the full weight of the town’s financial burdens. This level of taxation is untenable, unfair, and unsustainable. I urge you to consider the broader implications of this decision, not just for individual residents but for the future stability and prosperity of Hamden itself.\n\n",
  "Please know that we, the homeowners, are paying close attention. The outcome of this vote will not be forgotten when we cast our ballots in the primaries and general elections.\n",
  "I'm not alone in how I feel—check out this petition to see others who support this change.\n\n",
  "Sincerely,\n",
  "[Your Name]\n",
  "[Your Address]"
)
