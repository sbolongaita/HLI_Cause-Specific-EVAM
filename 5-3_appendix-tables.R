
### 5.3 Appendix tables



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()



# 2 Tab A1 Country groupings ----------------------------------------------

# Table name
table_name <- "TabA1_country-groupings.xlsx"

# Loading data
sarahLoad("country_info", folder = "data/processed")

# Prepping table
temp1 <- country_info %>%
  mutate(country = ifelse(frontier_eligible, paste(country, "*"), country)) %>%
  select(region, analysis_eligible, country) %>%
  arrange(region, analysis_eligible, country)

temp2 <- temp1 %>%
  group_by(region, analysis_eligible) %>%
  mutate(column = rep(LETTERS[1:4], length.out = n()))

temp3 <- temp2 %>%
  pivot_wider(id_cols = c(region, analysis_eligible), names_from = column, values_from = country, values_fn = length) %>%
  mutate_at(.vars = c("A", "B", "C", "D"), ~ ifelse(is.na(.), 0, .)) %>%
  rowwise() %>% mutate(max = max(A, B, C, D)) %>%
  mutate_at(.vars = c("A", "B", "C", "D"), ~ max - .) %>%
  select(-max) %>%
  pivot_longer(cols = c(A, B, C, D), names_to = "column", values_to = "country") %>%
  filter(country != 0) %>%
  mutate(country = NA)

temp4 <- bind_rows(temp2, temp3) %>%
  pivot_wider(id_cols = c(region, analysis_eligible), names_from = column, values_from = country, values_fn = list) %>%
  unnest(cols = c(A, B, C, D)) %>%
  filter(analysis_eligible)

temp5 <- temp4 %>%
  group_by(region) %>%
  group_modify(~ add_row(.x, .before = 0)) %>%
  mutate(A = ifelse(row_number() == 1, region, paste("   ", A))) %>%
  ungroup() %>%
  select(A, B, C, D)

table <- temp5

write.xlsx(table, file = paste("output/tables", table_name, sep = "/"),
           colNames = FALSE)



# 3 Tab A2 cause of death crosswalk ---------------------------------------

# Table name
table_name <- "TabA2_cause-crosswalk.xlsx"

# Loading data
sarahLoad("cause_recode_map", folder = "data/processed")

temp1 <- cause_recode_map %>%
  filter(level != 0, !is.na(recoded_causename)) %>%
  left_join(cause_hierarchy %>% select(recoded_ghecause = ghecause, prefix), by = "recoded_ghecause") %>%
  arrange(prefix) %>%
  select(level, recoded_causename, original_ghecause, original_causename) %>%
  group_by(recoded_causename) %>%
  mutate(n = row_number()) %>%
  mutate(level = ifelse(n != 1, NA, level),
         recoded_causename = ifelse(n != 1, NA, recoded_causename)) %>%
  ungroup() %>%
  mutate(recoded_causename = case_when(is.na(recoded_causename) ~ NA_character_,
                                       level == 2 ~ paste0(tab, recoded_causename),
                                       level == 3 ~ paste0(tab, tab, recoded_causename),
                                       TRUE ~ recoded_causename)) %>%
  select(Level = level, "Analysis cause" = recoded_causename, "GHE code" = original_ghecause, "GHE cause" = original_causename)

table <- temp1

write.xlsx(table, file = paste("output/tables", table_name, sep = "/"))



# 4 Tab A3 frontier projection method -------------------------------------

# Table name
table_name <- "TabA3_frontier-projection-method.xlsx"

# Loading data
sarahLoad("frontier_info_4", folder = "data/processed/frontier_info")

ages <- makeMathersAgeGroup(unique(frontier_info_4$age)) %>% unique()
ages <- ages[4:8]

temp1 <- frontier_info_4 %>%
  mutate(age2 = makeMathersAgeGroup(age)) %>%
  filter(year >= 2010 & year <= 2019, age2 %in% ages, definition == "10th percentile") %>%
  select(sex, age2, ghecause, causename, concern) %>% unique() %>%
  mutate(causename = case_when(sex == 1 ~ paste0(causename, " (males)"),
                               sex == 2 ~ paste0(causename, " (females)"),
                               TRUE ~ causename)) %>%
  left_join(cause_hierarchy %>% select(ghecause, level, prefix), by = "ghecause") %>%
  arrange(prefix, causename, age2, sex) %>%
  mutate(method = ifelse(is.na(concern), "OLS", "Average"))

symbols_used <- data.frame(item = str_split(paste0(unique(na.omit(temp1$concern)), collapse = "; "), "; ") %>%
                             unlist() %>% capitalize() %>% unique() %>% sort(),
                           symbol = symbols[1:3])

temp2 <- temp1 %>%
  mutate(method = paste0(method, " "))
for(i in 1:nrow(symbols_used)){
  search <- substr(symbols_used$item[i], 2, 100)
  temp2 %<>% mutate(method = ifelse(grepl(search, concern), paste0(method, symbols_used$symbol[i]), method))
}

temp3 <- temp2 %>%
  mutate(method = trimws(method, which = "both")) %>%
  pivot_wider(id_cols = c(level, causename), names_from = age2, values_from = method) %>%
  mutate(causename = case_when(level == 0 ~ causename,
                               level == 1 ~ paste0(tab, causename),
                               level == 2 ~ paste0(tab, tab, causename),
                               level == 3 ~ paste0(tab, tab, tab, causename)))

symbols_used %<>% mutate(level = paste(symbol, "=", item)) %>% pull(level) %>% paste(., collapse = new.line)

temp4 <- temp3 %>%
  mutate(level = as.character(level)) %>%
  bind_rows(data.frame(level = symbols_used)) %>%
  dplyr::rename("Level" = level, "Cause" = causename)

table <- temp4

write.xlsx(table, file = paste("output/tables", table_name, sep = "/"),
           colNames = TRUE)


# 5 Tab B1 Economic values -------------------------------------------------

applyEnv()

# Table name
table_name <- "TabB1_economic-values.xlsx"

# Loading data
sarahLoad("region_calculations", folder = "output/data")

# Prepping table
temp1 <- inner_join(cause_hierarchy %>% select(prefix, ghecause, level),
                    region_calculations, by = "ghecause") %>%
  filter(level == mece.lvl) %>%
  mutate(region = factor(region),
         sex = case_when(sex == 1 ~ "Males", sex == 2 ~ "Females", sex == 3 ~ "Total"),
         v.r = paste(format(round(v.r * 100, 1), nsmall = 1, trim = TRUE), "%")) %>%
  arrange(region, prefix) %>%
  pivot_wider(id_cols = c(prefix, level, causename, region), names_from = c(year, sex), values_from = v.r) %>%
  group_by(region) %>%
  group_modify(~ add_row(.x, .before = 0)) %>%
  ungroup() %>%
  mutate(causename = case_when(level == 1 ~ causename,
                               level == 2 ~ paste0(tab, causename),
                               level == 3 ~ paste0(tab, tab, causename)),
         level = ifelse(is.na(level), as.character(region), level)) %>%
  select(-c(region, prefix)) %>%
  add_row(.before = 0) %>%
  select(Level = level, `Cause of death` = causename,
         `2000 F` = `2000_Females`, `2000 M` = `2000_Males`, `2000 T` = `2000_Total`,
         `2019 F` = `2019_Females`, `2019 M` = `2019_Males`, `2019 T` = `2019_Total`,
         `2050 F` = `2050_Females`, `2050 M` = `2050_Males`, `2050 T` = `2050_Total`)

temp1[1, ] <- t(c(NA, NA, rep(c("Females", "Males", "Total"), 3)))

table <- temp1

write.xlsx(table, file = paste("output/tables", table_name, sep = "/"))
