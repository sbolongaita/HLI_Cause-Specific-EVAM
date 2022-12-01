
### 1.3 GHE Recode

# This script recodes the processed GHE cause of death data (limited to
# analysis countries in `1-1_country-eligibility.R`) so that causes of
# death are relevant for this analysis, mutually exclusive, and collectively
# exhaustive. This script relies a cause recode map created in `scr/ghe_recode.R`.


# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("ghe", "cause_recode_map"), folder = "data/processed")


# 2 Recoding causes -------------------------------------------------------

temp1 <- full_join(ghe,
                   cause_recode_map %>% rename(ghecause = original_ghecause, causename = original_causename),
                   by = c("ghecause", "causename"))

temp2 <- temp1 %>%
  filter(!is.na(recoded_ghecause)) %>%
  group_by(iso3, year, sex, age, recoded_ghecause, recoded_causename) %>%
  summarize(dths = sum(dths), .groups = "drop")

ghe_recoded <- temp2 %>%
  rename(ghecause = recoded_ghecause, causename = recoded_causename)

# 3 Checking recode -------------------------------------------------------

# Original causes, with unused lower level causes omitted, with deaths
# summed by country, year, age, and sex
original <- temp1 %>%
  filter(!is.na(recoded_ghecause)) %>%
  group_by(iso3, year, sex, age) %>%
  summarize(dths = sum(dths), .groups = "drop")

# Recoded causes with deaths summed by country, year, age, and sex
recoded <- temp2 %>%
  group_by(iso3, year, sex, age) %>%
  summarize(dths = sum(dths), .groups = "drop")

# Checking that deaths under original causes (with unused lower level causes
# omitted) equal deaths under recoded causes
check <- full_join(original, recoded, by = c("iso3", "year", "sex", "age")) %>%
  filter(round(dths.x - dths.y) != 0)

# __+ ghe_recoded ---------------------------------------------------------
sarahSave("ghe_recoded", folder = "data/processed")
