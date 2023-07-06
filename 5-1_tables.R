
### 5.1 Tables


# 1 Tab 1 Causes of death -------------------------------------------------

applyEnv()

# Table name
table_name <- "Tab1.xlsx"

# Loading data
sarahLoad("cause_hierarchy", folder = "data/processed")

# Prepping table
temp1 <- cause_hierarchy %>%
  filter(level > 0) %>%
  mutate(causename = ifelse(level == 2, paste0(tab, causename), as.character(causename))) %>%
  mutate(causename = ifelse(level == 3, paste0(tab, tab, causename), as.character(causename))) %>%
  select("Level" = level, "Cause of death" = causename)

table <- temp1

write.xlsx(table, file = paste("output/tables", table_name, sep = "/"))



# 2 Tab 2 Avoidable mortality ---------------------------------------------

applyEnv()

# Table name
table_name <- "Tab2.xlsx"

# Loading data
sarahLoad(c("country_info", "country_scaled", "frontier_scaled", "population"),
          folder = "data/processed")

# Focus years
focus <- c(2000, 2019, 2050)

# Prepping country data
country_scaled %<>%
  filter(year %in% focus) %>%
  mutate(sex_match = ifelse(causename %in% sex.specific, sex, NA)) %>%
  dplyr::select(iso3, year, age, sex_match, sex, ghecause, causename, dths_rate) %>%
  arrange(iso3, year, age, ghecause, sex)

# Prepping frontier data
frontier_scaled %<>%
  filter(definition == "10th percentile", year %in% focus) %>%
  dplyr::select(year, age, sex_match = sex, ghecause, causename, frontier) %>%
  arrange(year, age, ghecause, sex_match)

# Prepping population data
population %<>%
  filter(year %in% focus)

# Combining country and frontier data
temp1 <- full_join(country_scaled, frontier_scaled,
                    by = c("year", "age", "sex_match", "ghecause", "causename")) %>%
  mutate_at(vars(dths_rate, frontier), ~ . /100000) %>%
  dplyr::select(-sex_match) %>%
  arrange(iso3, ghecause, age, sex, year)

# Adding population and calculating deaths and unavoidable deaths
temp2 <- left_join(temp1, population, by = c("iso3", "year", "age", "sex")) %>%
  mutate(deaths = dths_rate * pop,
         unavoidable = frontier * pop)

# Summing by region and year
temp3 <- temp2 %>%
  group_by(region, ghecause, causename, year) %>%
  summarize(deaths = sum(deaths),
            unavoidable = sum(unavoidable)) %>%
  mutate(avoidable = ifelse(deaths - unavoidable > 0, deaths - unavoidable, 0))

# Checking regional sums equal world sums
check.region <- temp3 %>%
  filter(region != "World") %>%
  group_by(ghecause, causename, year) %>%
  summarize_at(vars(deaths, unavoidable, avoidable), ~sum(.)) %>%
  ungroup() %>%
  arrange(ghecause, year)
check.world <- temp3 %>%
  filter(region == "World") %>%
  ungroup() %>%
  select(-region) %>%
  arrange(ghecause, year)
all.equal(check.region, check.world)

# Recalculating world avoidable due to negative avoidable values that impact world sums
temp3.region <- temp3 %>%
  filter(region != "World")
temp3.world <- temp3 %>%
  filter(region != "World") %>%
  group_by(ghecause, causename, year) %>%
  summarize_at(vars(deaths, unavoidable, avoidable), ~sum(.)) %>%
  ungroup() %>%
  mutate(region = "World") %>%
  select(region, everything())
temp4 <- bind_rows(temp3.region, temp3.world)

# Rechecking
check.region <- temp4 %>%
  filter(region != "World") %>%
  group_by(ghecause, causename, year) %>%
  summarize_at(vars(deaths, unavoidable, avoidable), ~sum(.)) %>%
  ungroup() %>%
  arrange(ghecause, year)
check.world <- temp4 %>%
  filter(region == "World") %>%
  ungroup() %>%
  select(-region) %>%
  arrange(ghecause, year)
all.equal(check.region, check.world)

# Calculating avoidable percent
temp5 <- temp4 %>%
  mutate(avoidable.pct = avoidable / deaths)

# Prepping table
temp6 <- full_join(cause_hierarchy %>% select(level, ghecause, causename),
                   temp5, by = c("ghecause", "causename")) %>%
  select(region, year, level, ghecause, causename, avoidable, avoidable.pct) %>%
  arrange(region, year, ghecause) %>%
  mutate(causename = ifelse(level == 2, paste0(tab, causename), as.character(causename)),
         avoidable = format(round(avoidable), big.mark = ",", trim = TRUE),
         avoidable.pct = paste0(format(round(avoidable.pct * 100), trim = TRUE), "%")) %>%
  mutate(causename = ifelse(level == 3, paste0(tab, tab, causename), as.character(causename))) %>%
  pivot_wider(id_cols = c(region, ghecause, causename), names_from = year, values_from = c(avoidable, avoidable.pct)) %>%
  group_by(region) %>%
  group_modify(~add_row(.x, .before = 0)) %>%
  mutate(causename = ifelse(is.na(causename), region, causename)) %>%
  group_modify(~add_row(.x, .before = 0)) %>%
  ungroup() %>%
  mutate_all(~ifelse(is.na(.), "", .)) %>%
  select(causename, ends_with("2000"), ends_with("2019"), ends_with("2050"))

table <- temp6

write.xlsx(table, file = paste("output/tables", table_name, sep = "/"))



# 2 Tab 2 Economic values --------------------------------------------------

applyEnv()

# Table name
table_name <- "Tab3.xlsx"

# Loading data
sarahLoad("region_calculations", folder = "output/data")

# Calculating percentage of economic value
temp1 <- inner_join(cause_hierarchy %>% select(prefix, ghecause, level),
                    region_calculations, by = "ghecause") %>%
  filter(level == mece.lvl, sex == 3) %>%
  mutate(v.r = paste0(format(round(v.r * 100, 1), nsmall = 1, trim = TRUE), "%")) %>%
  arrange(region, prefix) %>%
  pivot_wider(id_cols = c(prefix, level, causename, region), names_from = year, values_from = v.r) %>%
  group_by(region) %>%
  group_modify(~ add_row(.x, .before = 0)) %>%
  ungroup() %>%
  mutate(causename = case_when(is.na(causename) ~ region,
                               level == 1 ~ causename,
                               level == 2 ~ paste0(tab, causename),
                               level == 3 ~ paste0(tab, tab, causename))) %>%
  select(-c(region, level, prefix)) %>%
  mutate_all(~ifelse(is.na(.), "", .))

table <- temp1

write.xlsx(table, file = paste("output/tables", table_name, sep = "/"))
