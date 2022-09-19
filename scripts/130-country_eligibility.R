
### Healthy Longevity Initiative
### 130 - COUNTRY ELIGIBILITY

# This script defines the eligibility criteria for country inclusion in the
# overall analysis and the frontier analysis, which has stricter inclusion
# criteria than the overall analysis. Countries were eligible for the
# analysis if they had populations of at least 5 million in 2020. Countries
# were eligible for the frontier analysis if they: had populations of at
# least 5 million in 2020, have high-quality vital registration data, and
# were not excluded from the GHE 2019. The script then applies the defined
# eligibility criteria to the GHE data.

# Returns:
# • output_data/country_info.R (R dataset)
# • output_data/ghe.R (R dataset)



# 1 ENVIRONMENT -----------------------------------------------------------

# Tidying and loading environment
sourceEnv(); tidy()
scriptName <- "130-country_eligibility"

# Loading data
names <- c("ghe", "population", "quality", "region")
for(name in names){
  files <- list.files("02-input_data", pattern = name, full.names = TRUE)
  if(length(files) > 1){
    dfs <- list()
    for(file in files){
      dfs[[file]] <- read.csv(file, as.is = TRUE)
    }
    df <- bind_rows(dfs)
    assign(name, df)
  }else{
    assign(name, read.csv(files, as.is = TRUE))
  }
}



# 2 INCLUSION CRITERIA ----------------------------------------------------

# Getting the starting list of countries (countries in GHE dataset) and
# combining with the HLI region data
temp1 <- data.frame(iso3 = sort(unique(ghe$iso3))) %>%
  left_join(region, by = "iso3") %>%
  mutate(country = getCountry(iso3)) %>%
  dplyr::select(region, iso3, country)


# * 2.1 Defining inclusion criteria ---------------------------------------

# Countries with high-quality data
hq_iso3 <- quality %>%
  filter(quality == 1) %>%
  arrange(iso3) %>% pull(iso3)

# Countries with populations of at least 5 million
pop_iso3 <- population %>%
  pivot_longer(cols = -c(iso3, year, sex), names_to = "age", values_to = "pop") %>%
  group_by(iso3, year) %>%
  dplyr::summarize(pop = sum(pop), .groups = "drop") %>%
  filter(year == 2020, pop >= 5000000) %>%
  pull(iso3) %>% unique()

# Countries that were not excluded from GHE 2019
nonghe_iso3 <- countryname(c("Belarus", "Belize", "Jamaica", "Kazakhstan",
                             "Kyrgyzstan", "Kuwait", "Trinidad and Tobago"),
                           destination = "iso3c")

# Adding this information to the dataframe
temp2 <- temp1 %>%
  mutate(pop_5mil = ifelse(iso3 %notin% pop_iso3, "2020 population less than 5 million", NA),
         high_quality = ifelse(iso3 %notin% hq_iso3, "Low quality vital registration data", NA),
         non_ghe2019 = ifelse(iso3 %in% nonghe_iso3, "Not included in GHE 2019", NA))

# Documenting eligibility for analysis and the frontier
temp3 <- temp2 %>%
  mutate(analysis_eligible = is.na(pop_5mil)) %>%
  unite(col = "frontier_exclusion", c(high_quality, pop_5mil, non_ghe2019),
        sep = "; ", na.rm = TRUE, remove = TRUE) %>%
  mutate(frontier_exclusion = capitalize(gsub("ghe", "GHE", tolower(frontier_exclusion)))) %>%
  mutate(frontier_exclusion = ifelse(frontier_exclusion == "", NA, frontier_exclusion)) %>%
  mutate(frontier_eligible = is.na(frontier_exclusion))

# Creating a data frame with information on all countries
country_info <- temp3 %>%
  dplyr::select(iso3, country, region, analysis_eligible, frontier_eligible, frontier_exclusion)

# __ + country_info -------------------------------------------------------
sarahSave("country_info")



# 3 APPLYING INCLUSION CRITERIA -------------------------------------------

# Creating a GHE dataset with analysis countries
ghe %<>%
  filter(iso3 %in% country_info$iso3[country_info$analysis_eligible])

# __+ ghe -----------------------------------------------------------------
sarahSave("ghe")



# 4 END -------------------------------------------------------------------

# Tidying environment and notifying the end of the script
notifyScript(); tidy()
