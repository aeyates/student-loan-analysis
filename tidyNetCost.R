## Tidy the Net Cost data for visualization, retaining only 2011/2020 data

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(janitor)


# Read Raw Data -----------------------------------------------------------
net_price_raw <- read_csv("raw-data/net-price.csv") %>% clean_names()


# Transformation and Tidying ----------------------------------------------

# Keep only 4-year colleges, transforming sector, pivot the average, dropping na
net_price_tidyish <- net_price_raw %>% 
  filter(sector_of_institution_hd2021 %in% c(1,2)) %>% 
  mutate(sector = ifelse(sector_of_institution_hd2021 == 1, 'Public', 'Private')) %>%
  select(-sector_of_institution_hd2021) %>% 
  pivot_longer(cols=-c(unit_id, institution_name, sector), names_to = "income_year") %>%
  drop_na(value)


# Income Data -------------------------------------------------------------

# Separate out the year, retaining only the income averages for 2011 and 2020
first_last_year_by_income <- net_price_tidyish %>%
  filter(str_detect(income_year, "average_net_price_students_awarded", TRUE)) %>% 
  separate_wider_regex(cols="income_year", patterns=c("average_net_price_income_", income = ".*", "_students_awarded_title_iv_federal_financial_aid_", year = ".*", "_sfa.*"), too_few = "align_start") %>%
  filter(year == '2011_12' | year == '2020_21')

# Determine which schools reported numbers for both years
complete_data_by_income <- first_last_year_by_income %>% 
  group_by(institution_name, sector, income) %>% 
  count() %>% 
  filter(n==2) %>% 
  select(-n)

tidy_by_income <- inner_join(first_last_year_by_income, complete_data_by_income)


# Overall Data ------------------------------------------------------------

# Separate out the year, retaining only the overall averages for 2011 and 2020
first_last_year <- net_price_tidyish %>% 
  filter(str_detect(income_year, "average_net_price_students_awarded", FALSE)) %>% 
  separate_wider_regex(cols="income_year", patterns=c("average_net_price_students_awarded_grant_or_scholarship_aid_", year = ".*", "_sfa.*"), too_few = "align_start") %>%
  filter(year == '2011_12' | year == '2020_21')

# Determine which schools reported numbers for both years
complete_data <- first_last_year %>% 
  group_by(institution_name, sector) %>% 
  count() %>% 
  filter(n==2) %>% 
  select(-n)

tidy_overall <- inner_join(first_last_year, complete_data)

# Write Data --------------------------------------------------------------

write_rds(file = "data/net_cost_2011_2020_by_income.rds", tidy_by_income)
write_rds(file = "data/net_cost_2011_2020_overall.rds", tidy_overall)
