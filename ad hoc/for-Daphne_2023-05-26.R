
# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("cause_hierarchy", "frontier_harmonized", "population", "country_scaled"),
          folder = "data/processed")

# World Bank income classifications
income <- read.xlsx("data/raw/CLASS.xlsx")



# NCD mortality rates in LMICs --------------------------------------------

temp1 <- income %>%
  mutate(iso3 = countrycode(Code, origin = "wb", destination = "iso3c")) %>%
  select(iso3, income = Income.group) %>%
  filter(!is.na(iso3), !is.na(income),
         income %in% c("Low income", "Lower middle income", "Upper middle income")) %>%
  arrange(iso3)

lmic <- country_scaled %>%
  filter(iso3 %in% temp1$iso3, causename == "Noncommunicable diseases") %>%
  left_join(population %>% select(-region), by = join_by(iso3, year, sex, age)) %>%
  mutate(dths = (dths_rate / 100000) * pop) %>%
  group_by(year, ghecause, causename) %>%
  summarize_at(vars(dths, pop), ~ sum(.)) %>%
  mutate(dths_rate = dths / pop * 100000)


# NCD mortality rates in 20th percentile ----------------------------------

temp2 <- frontier_harmonized %>%
  filter(definition == "20th percentile", causename == "Noncommunicable diseases")

p20 <- temp2 %>%
  select(-sex) %>%
  left_join(country_scaled, by = join_by(year, age, ghecause, causename)) %>%
  filter(dths_rate <= frontier)
  # left_join(population %>% select(-region), by = join_by(iso3, year, sex, age)) %>%
  # mutate(dths = (dths_rate / 100000) * pop) %>%
  # group_by(year, ghecause, causename) %>%
  # summarize_at(vars(dths, pop), ~ sum(.)) %>%
  # mutate(dths_rate = dths / pop * 100000)
