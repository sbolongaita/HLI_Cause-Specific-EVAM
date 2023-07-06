
### 1.1 Country eligibility

# This script defines and applies the eligibility criteria for country
# inclusion in the analysis. Countries were eligible for inclusion if they
# had populations of at least five million in 2019 and available income
# (i.e., GNI per capita) data for 2019. Of those analysis-eligible countries,
# a smaller subset were eligible for the frontier analysis: those with
# high-quality vital registration data and which were not excluded from
# the GHE 2019 analysis.



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
names <- c("ghe", "gni", "population", "quality", "region")
for(name in names){
  files <- list.files("data/input", pattern = name, full.names = TRUE)
  if(length(files) > 1){
    dfs <- list()
    for(file in files){
      dfs[[file]] <- read.csv(file, as.is = TRUE)
      cat(paste(file, "loaded\n"))
    }
    df <- bind_rows(dfs)
    assign(name, df)
  }else{
    assign(name, read.csv(files, as.is = TRUE))
  }
}



# 2 Defining inclusion criteria -------------------------------------------

# Extracting the starting list of countries (countries in the GHE dataset)
# and combining with the HLI region data
temp1 <- data.frame(iso3 = sort(unique(ghe$iso3))) %>%
  left_join(region %>% filter(region != "World"), by = "iso3") %>%
  mutate(country = getCountry(iso3)) %>%
  dplyr::select(region, iso3, country)

# Countries with populations of at least 5 million
pop_iso3 <- population %>%
  group_by(iso3, year) %>%
  dplyr::summarize(pop = sum(pop), .groups = "drop") %>%
  filter(year == 2019, pop >= 5000000) %>%
  pull(iso3) %>% unique()

# Countries with high-quality data
hq_iso3 <- quality %>%
  filter(quality == 1) %>%
  arrange(iso3) %>% pull(iso3)

# Countries that were not excluded from GHE 2019
nonghe_iso3 <- countryname(c("Belarus", "Belize", "Jamaica", "Kazakhstan",
                             "Kyrgyzstan", "Kuwait", "Trinidad and Tobago"),
                           destination = "iso3c")

# Countries that have GNI per capita data
gni_iso3 <- gni %>%
  filter(year == 2019, !is.na(gni.pc)) %>%
  arrange(iso3) %>% pull(iso3) %>% unique()

# Adding this information to the dataframe
temp2 <- temp1 %>%
  mutate(pop = ifelse(iso3 %notin% pop_iso3, "2019 population less than 5 million", NA),
         qual = ifelse(iso3 %notin% hq_iso3, "Low quality vital registration data", NA),
         ghe = ifelse(iso3 %in% nonghe_iso3, "Not included in GHE 2019", NA),
         gni = ifelse(iso3 %notin% gni_iso3, "2019 income data unavailable", NA))

# Documenting eligibility for analysis and the frontier
temp3 <- temp2 %>%
  mutate(analysis_eligible = is.na(pop) & is.na(gni)) %>%
  unite(col = "frontier_exclusion", c(pop, qual, ghe, gni),
        sep = "; ", na.rm = TRUE, remove = TRUE) %>%
  mutate(frontier_exclusion = capitalize(gsub("ghe", "GHE", tolower(frontier_exclusion)))) %>%
  mutate(frontier_exclusion = ifelse(frontier_exclusion == "", NA, frontier_exclusion)) %>%
  mutate(frontier_eligible = is.na(frontier_exclusion))

# Creating a data frame with information on all countries
country_info <- temp3 %>%
  dplyr::select(iso3, country, region, analysis_eligible, frontier_eligible, frontier_exclusion) %>%
  ungroup()

# __ + country_info -------------------------------------------------------
sarahSave("country_info", folder = "data/processed")



# 3 Applying inclusion criteria -------------------------------------------

# Creating a GHE dataset with analysis countries
ghe %<>%
  filter(iso3 %in% country_info$iso3[country_info$analysis_eligible]) %>%
  ungroup()

# __+ ghe -----------------------------------------------------------------
sarahSave("ghe", folder = "data/processed")
