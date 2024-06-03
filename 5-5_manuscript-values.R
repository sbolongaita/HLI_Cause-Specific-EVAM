
### 5.5 Nature Medicine manuscript values

applyEnv()
sarahLoad("region_calculations", folder = "output/data")

data <- inner_join(cause_hierarchy %>% select(prefix, ghecause, level),
                    region_calculations, by = "ghecause") %>%
  filter(year == 2019, level == mece.lvl) %>%
  mutate(sex = case_when(sex == 1 ~ "Males",
                         sex == 2 ~ "Females",
                         sex == 3 ~ "Total"),
         v.r = round(v.r * 100, 1))



# Abstract ----------------------------------------------------------------

# CVD 2019
data %>%
  filter(causename == "Cardiovascular diseases", sex != "Total", region != "World") %>%
  arrange(v.r)

# Cancers 2019
data %>%
  filter(causename == "Malignant neoplasms", sex == "Total", region %in% c("China", "High-income")) %>%
  arrange(v.r)

# Intentional injuries 2019
data %>%
  filter(causename == "Intentional injuries", sex != "Total", region != "World") %>%
  arrange(v.r)

# Unintentional injuries 2019
data %>%
  filter(causename == "Unintentional injuries", sex != "Total", region != "World") %>%
  arrange(v.r)


# Results / Economic value of avoidable mortality -------------------------

# CVD + diabetes 2019
data %>%
  filter(causename %in% c("Cardiovascular diseases", "Diabetes mellitus"), sex == "Total", region != "World") %>%
  group_by(region) %>%
  summarize(v.r = sum(v.r)) %>%
  arrange(v.r)

data %>%
  filter(causename %in% c("Cardiovascular diseases", "Diabetes mellitus"), sex != "Total", region != "World") %>%
  group_by(region, sex) %>%
  summarize(v.r = sum(v.r)) %>%
  arrange(region, sex)

# Malignant neoplasms by sex 2019
data %>%
  filter(causename == "Malignant neoplasms", sex != "Total", region != "World") %>%
  arrange(region, sex)

# Injuries by sex 2019
data %>%
  filter(causename == "Injuries", sex != "Total", region != "World") %>%
  arrange(v.r)

# Intentional injuries 2019
data %>%
  filter(causename == "Intentional injuries", sex != "Total", region != "World") %>%
  arrange(v.r)

# Unintentional injuries 2019
data %>%
  filter(causename == "Unintentional injuries", sex != "Total", region != "World") %>%
  arrange(v.r)

# Injuries, other regions, 2019
data %>%
  filter(causename == "Injuries", sex != "Total", region %notin% c("Latin America & Caribbean", "Sub-Saharan Africa", "World")) %>%
  arrange(sex, desc(v.r))

# Communicable by sex 2019
data %>%
  filter(causename == "Communicable, maternal, perinatal and nutritional conditions", sex != "Total", region != "World") %>%
  arrange(desc(v.r))



# Results / Uncertainty and sensitivity analyses --------------------------

sarahLoad("SA_region_calculations", folder = "output/data")

SA_data <- inner_join(cause_hierarchy %>% select(prefix, ghecause, level),
                      SA_region_calculations, by = "ghecause") %>%
  filter(year == 2019, level == mece.lvl) %>%
  mutate(sex = case_when(sex == 1 ~ "Males",
                         sex == 2 ~ "Females",
                         sex == 3 ~ "Total"),
         v.r = round(v.r * 100, 1))

# Minimum frontier definition, CVD 2019
SA_data %>%
  filter(scenario == "Minimum frontier definition", region != "World", year == 2019, sex == "Total", causename == "Cardiovascular diseases") %>%
  arrange(desc(v.r))

# Lower or higher income elasticity, CVD 2019
SA_data %>%
  filter(scenario %in% c("Lower income elasticity", "Higher income elasticity"), region != "World", year == 2019,
         sex == "Total", causename == "Cardiovascular diseases") %>%
  pivot_wider(id_cols = c(prefix, ghecause, region, year, sex, causename, mece.lvl), names_from = scenario, values_from = v.r) %>%
  arrange(desc(`Higher income elasticity`))

# OECD base income, CVD 2019
SA_data %>%
  filter(scenario == "OECD base income", region != "World", year == 2019, sex == "Total", causename == "Cardiovascular diseases") %>%
  arrange(desc(v.r))

# Lower or higher discount rates, CVD 2019
SA_data %>%
  filter(scenario %in% c("Lower discount rate", "Higher discount rate"), region != "World", year == 2019,
         sex == "Total", causename == "Cardiovascular diseases") %>%
  pivot_wider(id_cols = c(prefix, ghecause, region, year, sex, causename, mece.lvl), names_from = scenario, values_from = v.r) %>%
  select(prefix:mece.lvl, `Lower discount rate`, `Higher discount rate`) %>%
  arrange(desc(`Lower discount rate`))

# Table
bind_rows(data %>% filter(year == 2019, sex == "Total", causename == "Cardiovascular diseases"),
                   SA_data %>% filter(year == 2019, sex == "Total", causename == "Cardiovascular diseases")) %>%
  mutate(scenario = ifelse(is.na(scenario), "Base", scenario)) %>%
  pivot_wider(id_cols = region, names_from = scenario, values_from = v.r) %>%
  mutate(`Alternate discount rates` = paste(`Lower discount rate`, "-", `Higher discount rate`),
         `Alternate income elasticities` = paste(`Higher income elasticity`, "-", `Lower income elasticity`)) %>%
  select(region, Base, `Minimum frontier definition`, `Alternate income elasticities`, `OECD base income`, `Alternate discount rates`)



# Discussion --------------------------------------------------------------

# CVD 2019
data %>%
  filter(causename == "Cardiovascular diseases", sex == "Total", region == "World")

# CVD + diabetes 2019
data %>%
  filter(causename %in% c("Cardiovascular diseases", "Diabetes mellitus"), region != "World") %>%
  group_by(region, sex) %>%
  summarize(v.r = sum(v.r)) %>%
  arrange(region, sex != "Total", sex)

# CVD and diabetes 2019
data %>%
  filter(causename %in% c("Cardiovascular diseases", "Diabetes mellitus")) %>%
  arrange(desc(v.r)) %>%
  pivot_wider(id_cols = c(region, causename), names_from = sex, values_from = v.r) %>%
  arrange(causename, region) %>%
  mutate(cp = paste0(region, ": ", Females, " (F), ", Males, " (M), ", Total, " (T)"))
