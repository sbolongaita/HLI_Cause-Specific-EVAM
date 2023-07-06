
### 2.1 Frontier Definition

# This script takes the recoded GHE data for frontier-eligible countries,
# calculates age-cause-specific mortality rates, and the extracts the frontier
# using three definitions: the minimum, the 10th percentile, and the 20th
# percentile. It creates a `data/processed/frontier_info` folder, in which
# the 'frontier' is tracked throughout each subsequent step of its
# calculation (i.e., scripts with prefix 2-).



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("cause_hierarchy", "country_info", "ghe_recoded", "population"),
          folder = "data/processed")



# 2 Extracting the frontier -----------------------------------------------

# Calculating age-cause-sex-year-specific mortality rates for frontier-eligible
# countries
ghe_recoded %<>%
  left_join(population %>% filter(region != "World") %>% select(iso3, year, sex, age, pop),
            by = c("iso3", "year", "sex", "age")) %>%
  mutate(dths_rate = dths / pop * 100000)

# Extracting the minimum
frontier_min <- ghe_recoded %>%
  mutate(sex = ifelse(causename %in% sex.specific, sex, NA)) %>%
  group_by(year, age, ghecause, causename, sex) %>%
  dplyr::summarize(frontier = min(dths_rate), .groups = "drop") %>%
  dplyr::select(year, sex, age, ghecause, causename, everything()) %>%
  arrange(year, age, ghecause, sex)

# Extracting the 10th percentile
frontier_10p <- ghe_recoded %>%
  mutate(sex = ifelse(causename %in% sex.specific, sex, NA)) %>%
  group_by(year, age, ghecause, causename, sex) %>%
  dplyr::summarize(frontier = unname(quantile(dths_rate, probs = 0.1, type = 3)),
                   .groups = "drop") %>%
  dplyr::select(year, sex, age, ghecause, causename, everything()) %>%
  arrange(year, age, ghecause, sex)

# Extracting the 20th percentile
frontier_20p <- ghe_recoded %>%
  mutate(sex = ifelse(causename %in% sex.specific, sex, NA)) %>%
  group_by(year, age, ghecause, causename, sex) %>%
  dplyr::summarize(frontier = unname(quantile(dths_rate, probs = 0.2, type = 3)),
                   .groups = "drop") %>%
  dplyr::select(year, sex, age, ghecause, causename, everything()) %>%
  arrange(year, age, ghecause, sex)

# Combining minimum and 10th percentile frontiers
frontier_base <- bind_rows(frontier_min %>% mutate(definition = "Minimum"),
                           frontier_10p %>% mutate(definition = "10th percentile"),
                           frontier_20p %>% mutate(definition = "20th percentile")) %>%
  select(year, sex, age, ghecause, causename, definition, frontier) %>%
  arrange(definition, year, age, ghecause, sex) %>%
  ungroup()

# __+ frontier_base -------------------------------------------------------
sarahSave("frontier_base", folder = "data/processed")



# 3 Creating a frontier info dataframe ------------------------------------
# Documenting information about the frontier

frontier_info_1 <- ghe_recoded %>%
  dplyr::mutate(sex_match = ifelse(causename %in% sex.specific, sex, NA)) %>%
  left_join(frontier_base %>% rename(sex_match = sex),
            by = c("year", "sex_match", "age", "ghecause", "causename")) %>%
  filter(dths_rate == frontier) %>%
  mutate(iso3.sex = paste(iso3, sex)) %>%
  group_by(year, age, sex_match, ghecause, causename, definition, frontier) %>%
  summarize(n = n(), iso3.sex = paste(sort(unique(iso3.sex)), collapse = ", "),
            .groups = "drop") %>%
  mutate(iso3.sex = ifelse(n > 5, "5+ country-sex groups", iso3.sex)) %>%
  select(year, sex = sex_match, age, ghecause, causename, definition, base = frontier, iso3.sex) %>%
  arrange(definition, age, ghecause, sex, year) %>%
  ungroup()

# __+ frontier_info --------------------------------------------------------
if(!dir.exists("data/processed/frontier_info")){
  dir.create("data/processed/frontier_info")
}
sarahSave("frontier_info_1", folder = "data/processed/frontier_info")
