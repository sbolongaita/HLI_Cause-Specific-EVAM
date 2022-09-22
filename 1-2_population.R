
### 1.2 Population

# This script filters the input population data to the countries of interest
# based on the exclusion criteria applied in `1-1_country-eligibility.R`. It
# then calculates the total population for countries and regions (i.e., not
# stratified by age and sex), as well alpha (i.e., the proportion of the
# total population that a given age-sex group comprises).



# 1.  Loading data --------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad("country_info", folder = "data/processed")
population <- read.csv("data/input/population.csv", as.is = TRUE)



# 2.  Filtering to analysis-relevant countries ----------------------------

# Filtering population data to countries and years of interest and reformatting
# to long data
population <- population %>%
  filter(iso3 %in% country_info$iso3[country_info$analysis_eligible])


# 3.  Calculating total populations ---------------------------------------

# Calculating total population by country and year
country_population <- population %>%
  dplyr::group_by(iso3, year) %>%
  dplyr::mutate(total_pop = sum(pop)) %>%
  mutate(alpha = pop / total_pop)

# Calculating total population by region and year
temp1 <- left_join(population, country_info %>% dplyr::select(iso3, region),
                   by = "iso3") %>%
  dplyr::group_by(region, year, sex, age) %>%
  dplyr::summarize(pop = sum(pop), .groups = "drop")

region_population <- temp1 %>%
  dplyr::group_by(region, year) %>%
  dplyr::mutate(total_pop = sum(pop)) %>%
  mutate(alpha = pop / total_pop)



# 4.  Combining country and region population data ------------------------

population <- bind_rows(country_population %>% dplyr::rename(iso3.region = iso3),
                        region_population %>% dplyr::rename(iso3.region = region))

# __+ population ----------------------------------------------------------
sarahSave("population", folder = "data/processed")
