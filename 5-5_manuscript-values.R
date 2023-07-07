
# Manuscript values

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
  filter(causename == "Cardiovascular diseases", sex == "Total", region != "World") %>%
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
