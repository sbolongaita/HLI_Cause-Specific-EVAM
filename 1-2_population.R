
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
temp1 <- left_join(country_info %>% select(iso3, region), population,
                   by = "iso3")

population <- temp1 %>%
  dplyr::select(iso3, region, year, age, sex, pop) %>%
  arrange(iso3, year, sex, age)


# __+ population ----------------------------------------------------------
sarahSave("population", folder = "data/processed")
