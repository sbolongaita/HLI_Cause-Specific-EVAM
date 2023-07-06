
### 4.2 Sensitivity analyses

# This script runs sensitivity analyses.


# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("country_info", "country_scaled", "frontier_scaled", "population"),
          folder = "data/processed")
envelope <- read.csv("data/input/chang_envelope.csv", as.is = TRUE)

# Defining focus years
focus <- c(2000, 2019, 2050)



# 2 Mortality differential ------------------------------------------------

# Prepping country data
country_scaled %<>%
  filter(year %in% focus) %>%
  mutate(sex_match = ifelse(causename %in% sex.specific, sex, NA)) %>%
  dplyr::select(iso3, year, age, sex_match, sex, ghecause, causename, dths_rate) %>%
  arrange(iso3, year, age, ghecause, sex)

# Prepping frontier data
frontier_scaled %<>%
  filter(year %in% focus) %>%
  dplyr::select(year, age, sex_match = sex, ghecause, causename, definition, frontier) %>%
  arrange(year, age, ghecause, sex_match)

# Prepping population data
population %<>%
  filter(year %in% focus) %>%
  arrange(region, iso3, year)

# Combining country and frontier data
data <- inner_join(country_scaled, frontier_scaled,
                   by = c("year", "age", "sex_match", "ghecause", "causename")) %>%
  mutate_at(vars(dths_rate, frontier), ~ . /100000) %>%
  dplyr::select(-sex_match) %>%
  arrange(definition, iso3, ghecause, age, sex, year)

# Checking data
containsNA(data)



# 3 Calculations ----------------------------------------------------------

# * delta -----------------------------------------------------------------
# Difference between frontier mortality and country mortality
delta <- data %>%
  mutate(delta = exp(-1 * frontier) - exp(-1 * dths_rate)) %>%
  mutate(delta = ifelse(delta < 0, 0, delta))

# Checking data
containsNA(delta)


# * p ---------------------------------------------------------------------
# Proportion of avoidable mortality by cause

temp1 <- delta %>%
  left_join(cause_hierarchy %>% select(ghecause, starts_with("mece")),
            by = "ghecause") %>%
  pivot_longer(cols = starts_with("mece"), names_to = "mece.lvl", values_to = "mece.ind") %>%
  mutate(mece.lvl = as.factor(as.numeric(substr(mece.lvl, nchar(mece.lvl), nchar(mece.lvl)))))

temp2 <- list()
for(i in unique(temp1$mece.lvl)){
  temp2[[i]] <- temp1 %>%
    filter(mece.lvl == i, mece.ind) %>%
    group_by(definition, iso3, year, age, sex, mece.lvl) %>%
    mutate(Delta = sum(delta)) %>%
    ungroup() %>% select(-mece.ind)
  if(i == tail(unique(temp1$mece.lvl), 1)){
    temp2 <- bind_rows(temp2)
  }
}

p <- temp2 %>%
  mutate(p = delta / Delta) %>%
  select(definition, iso3, year, age, sex, ghecause, causename, mece.lvl, everything()) %>%
  arrange(definition, iso3, sex, age, ghecause, mece.lvl)

# Checking data
# - Verifying p sums to 1
check <- p %>%
  group_by(definition, iso3, year, age, sex, mece.lvl) %>%
  summarize(p = sum(p), .groups = "drop")
check %>% filter(round(p, 4) != 1)
# - Checking NA, NAN, infinite
containsNA(p)

p <- p %>% mutate(p = ifelse(is.nan(p), 0, p))
# - Re-checking NA, NAN, infinite
containsNA(p)


# * alpha ------------------------------------------------------------------

# Age-sex weights
alpha <- population %>%
  filter(region != "World") %>%
  group_by(iso3, year, sex) %>%
  mutate(alpha = pop / sum(pop)) %>%
  ungroup() %>%
  select(iso3, year, age, sex, alpha)

# Checking data
# = Verifying alpha sums to 1
check <- alpha %>%
  group_by(iso3, year, sex) %>%
  summarize(alpha = sum(alpha), .groups = "drop")
check %>% filter(round(alpha, 4) != 1)
# - Checking NA, NAN, infinite
containsNA(alpha)
# - Checking consistency w envelope
check <- alpha %>%
  left_join(envelope %>%
              select("iso3", "year", "age", "sex", "pop.sex.weight"),
            by = c("iso3", "year", "age", "sex"))
check %>% filter(round(alpha, 4) != round(pop.sex.weight, 4))

alpha <- p %>% left_join(alpha, by = c("iso3", "year", "age", "sex"))


# * v.c --------------------------------------------------------------------

v.country <- inner_join(alpha, envelope %>%
                          select("scenario" ,"iso3", "year", "age", "sex", "b_log_1yr"),
                        by = c("iso3", "year", "age", "sex")) %>%
  filter((definition == "Minimum" & scenario == "Base") | (definition == "10th percentile" & scenario != "Base")) %>%
  mutate(scenario = ifelse(scenario == "Base", "Minimum frontier definition", scenario)) %>%
  mutate(v.c = alpha * p * b_log_1yr) %>%
  group_by(scenario, iso3, year, sex, ghecause, causename, mece.lvl) %>%
  summarize(v.c = sum(v.c), .groups = "drop") %>%
  arrange(scenario, iso3, year, sex, mece.lvl, ghecause)

SA_country_calculations <- v.country

# __ + SA_country_calculations --------------------------------------------
sarahSave("SA_country_calculations", folder = "data/processed")
sarahSave("SA_country_calculations", folder = "output/data")
write.csv(SA_country_calculations, file = "output/data/country_calculations.csv",
          na = "", row.names = FALSE)


# * w ---------------------------------------------------------------------
# Country weights

country.w <- population %>%
  filter(iso3 %in% unique(v.country$iso3)) %>%
  group_by(region, iso3, year, sex) %>%
  summarize(pop = sum(pop), .groups = "drop") %>%
  group_by(region, year, sex) %>%
  mutate(w = pop / sum(pop)) %>%
  ungroup() %>% select(-pop)

# Checking data
# = Verifying alpha sums to 1
check <- country.w %>%
  group_by(region, year, sex) %>%
  summarize(w = sum(w), .groups = "drop")
check %>% filter(round(w, 4) != 1)
# - Checking NA, NAN, infinite
containsNA(country.w)

w <- v.country %>% left_join(country.w, by = c("iso3", "year", "sex")) %>%
  select(scenario, region, iso3, everything()) %>%
  arrange(region, iso3)

containsNA(w)


# * v.r --------------------------------------------------------------------

v.region <- w %>%
  mutate(v.r = w * v.c) %>%
  group_by(scenario, region, year, sex, ghecause, causename, mece.lvl) %>%
  summarize(v.r = sum(v.r), .groups = "drop") %>%
  arrange(scenario, region, year, sex, mece.lvl, ghecause)

# Checking data
# = Verifying China and India v.c = v.r
check <- v.country %>% filter(iso3 %in% c("CHN", "IND")) %>%
  mutate(region = ifelse(iso3 == "CHN", "China", "India")) %>%
  left_join(v.region,
            by = c("scenario", "year", "sex", "ghecause", "causename", "mece.lvl", "region"))
check %>% filter(v.c != v.r)
# - Checking NA, NAN, infinite
containsNA(v.region)


# Adding total (sex-weighting)

# Sex weights
sex.w <- population %>%
  filter(iso3 %in% unique(v.country$iso3)) %>%
  group_by(region, year, sex) %>%
  summarize(pop = sum(pop), .groups = "drop") %>%
  group_by(region, year) %>%
  mutate(w = pop / sum(pop)) %>%
  ungroup() %>% select(-pop)

temp1 <- v.region %>%
  left_join(sex.w, by = join_by(region, year, sex)) %>%
  mutate(v.r = v.r * w) %>%
  group_by(scenario, region, year, ghecause, causename, mece.lvl) %>%
  summarize(v.r = sum(v.r), .groups = "drop") %>%
  mutate(sex = 3)

v.region <- bind_rows(v.region, temp1) %>%
  arrange(scenario, region, year, sex)

check <- v.region %>%
  pivot_wider(id_cols = c(scenario, region, year, ghecause, causename, mece.lvl), names_from = sex, values_from = v.r) %>%
  mutate(check = `3` < `2` & `3` > `1` | `3` < `1` & `3` > `2`)



# * R_bar -----------------------------------------------------------------
R_bar <- v.region %>%
  group_by(scenario, region, year, sex, mece.lvl) %>%
  dplyr::mutate(R_bar = v.r / sum(v.r))

# Checking data
# = Verifying alpha sums to 1
check <- R_bar %>%
  group_by(scenario ,region, year, sex, mece.lvl) %>%
  summarize(R_bar = sum(R_bar), .groups = "drop")
check %>% filter(round(R_bar, 4) != 1)
# - Checking NA, NAN, infinite
containsNA(R_bar)

SA_region_calculations <- R_bar


# __ + SA_region_calculations ---------------------------------------------
sarahSave("SA_region_calculations", folder = "data/processed")
sarahSave("SA_region_calculations", folder = "output/data")
write.csv(SA_region_calculations, file = "output/data/region_calculations.csv",
          na = "", row.names = FALSE)


# roc ---------------------------------------------------------------------

SA_roc <- SA_region_calculations %>%
  pivot_wider(id_cols = c(scenario, region, sex, ghecause, causename, mece.lvl),
              names_from = year, values_from = v.r) %>%
  mutate(roc1 = (`2019` / `2000`)^(1/(2019-2000)) - 1,
         roc2 = (`2050` / `2019`)^(1/(2050-2019)) - 1) %>%
  select(scenario, region, sex, ghecause, causename, mece.lvl, roc1, roc2)

# __ + SA_roc -------------------------------------------------------------
write.xlsx(SA_roc, file = "output/data/roc.xlsx")
sarahSave("SA_roc", folder = "data/processed")
sarahSave("SA_roc", folder = "output/data")
