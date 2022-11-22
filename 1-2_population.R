
### 1.2 Population

# This script filters the input population data to the countries of interest
# based on the inclusion criteria applied in `1-1_country-eligibility.R`.



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad("country_info", folder = "data/processed")
population <- read.csv("data/input/population.csv", as.is = TRUE)



# 2 Filtering to analysis countries ---------------------------------------

# Filtering population data to countries and years of interest and reformatting
# to long data
population <- country_info %>%
  filter(analysis_eligible) %>%
  select(region, iso3) %>%
  left_join(population, by = "iso3") %>%
  dplyr::select(region, iso3, year, age, sex, pop) %>%
  arrange(iso3, year, sex, age)


# __+ population ----------------------------------------------------------
sarahSave("population", folder = "data/processed")
