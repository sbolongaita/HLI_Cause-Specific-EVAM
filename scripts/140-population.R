
### Healthy Longevity Initiative
### 140 POPULATION

# This script takes UN Population population data and produces cleaned
# analysis-relevant country- and region-level population data.

# Returns:
# • data/processed/population.R (R dataset)



# 1 ENVIRONMENT -----------------------------------------------------------

# Script name
scriptName <- "120-population.R"

# Loading data
sarahLoad("country_info", folder = "data/processed")
population <- read.csv("data/input/population.csv", as.is = TRUE)



# 2 POPULATION ------------------------------------------------------------

# Filtering population data to countries and years of interest and reformatting
# to long data
population <- population %>%
  filter(iso3 %in% country_info$iso3[country_info$analysis_eligible])


# * 2.1 Country-level population ------------------------------------------

# Calculating total population by country and year
country_population <- population %>%
  dplyr::group_by(iso3, year) %>%
  dplyr::mutate(total_pop = sum(pop)) %>%
  mutate(alpha = pop / total_pop)


# * 2.2 Region-level population -------------------------------------------

# Calculating region-level population according to HLI region
temp1 <- left_join(population, country_info %>% dplyr::select(iso3, region),
                   by = "iso3") %>%
  dplyr::group_by(region, year, sex, age) %>%
  dplyr::summarize(pop = sum(pop), .groups = "drop")

# Calculating total population by region and year
region_population <- temp1 %>%
  dplyr::group_by(region, year) %>%
  dplyr::mutate(total_pop = sum(pop)) %>%
  mutate(alpha = pop / total_pop)


# * 2.4 Combining country and region levels -------------------------------

population <- bind_rows(country_population %>% dplyr::rename(iso3.region = iso3),
                        region_population %>% dplyr::rename(iso3.region = region))

# __+ population ----------------------------------------------------------
sarahSave("population", folder = "data/processed")



# 3 END -------------------------------------------------------------------

# Tidying environment and notifying the end of the script
notifyScript(); tidy()
