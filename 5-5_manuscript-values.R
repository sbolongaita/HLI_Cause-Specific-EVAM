
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
  filter(causename == "Cardiovascular diseases", sex != "Total", region != "World") %>%
  arrange(v.r)

# Cancers 2019
data %>%
  filter(causename == "Malignant neoplasms", sex != "Total", region != "World") %>%
  arrange(desc(v.r)) %>%
  head()

# Injuries 2019
data %>%
  filter(causename == "Injuries", sex != "Total", region != "World") %>%
  arrange(desc(v.r)) %>%
  head()


# Results / Economic value of avoidable mortality -------------------------

# CVD + diabetes 2019
data %>%
  filter(causename %in% c("Cardiovascular diseases", "Diabetes mellitus"), sex == "Total", region != "World") %>%
  group_by(region) %>%
  summarize(v.r = sum(v.r)) %>%
  arrange(desc(v.r))

data %>%
  filter(causename %in% c("Cardiovascular diseases", "Diabetes mellitus"), region != "World") %>%
  group_by(region, sex) %>%
  summarize(v.r = sum(v.r)) %>%
  arrange(region, sex != "Total", sex)

# Malignant neoplasms by sex 2019
data %>%
  filter(causename == "Malignant neoplasms", sex != "Total", region != "World") %>%
  arrange(desc(v.r))

# Injuries by sex 2019
data %>%
  filter(causename == "Injuries", region %in% c("Latin America & Caribbean", "Sub-Saharan Africa")) %>%
  arrange(region, sex)

# Intentional injuries 2019
data %>%
  filter(causename == "Intentional injuries", region %in% c("Latin America & Caribbean", "Sub-Saharan Africa")) %>%
  arrange(region, sex)

# Unintentional injuries 2019
data %>%
  filter(causename == "Unintentional injuries", region %in% c("Latin America & Caribbean", "Sub-Saharan Africa")) %>%
  arrange(region, sex)

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
