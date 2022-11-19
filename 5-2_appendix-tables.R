
### 5.2 Appendix tables



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Creating tables objects
tables <- list()



# 2 Tab A1 country groupings ----------------------------------------------

# Table name
table_name <- "TabA1.xlsx"

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
  unnest(cols = c(A, B, C, D))

temp5 <- temp4 %>%
  group_by(region, analysis_eligible) %>%
  group_modify(~ add_row(.x, .before = 0)) %>%
  arrange(region, desc(analysis_eligible), !is.na(A)) %>%
  filter(!(row_number() == 1 & analysis_eligible)) %>%
  mutate(A = case_when(row_number() == 1 & !analysis_eligible ~ "Not included in the analysis:",
                       !is.na(A) ~ paste0(tab, A),
                       TRUE ~ NA_character_)) %>%
  group_by(region) %>%
  group_modify(~ add_row(.x, .before = 0)) %>%
  arrange(region, !is.na(A)) %>%
  mutate(A = ifelse(row_number() == 1, region, A)) %>%
  ungroup() %>%
  select(A, B, C, D)

tables[[table_name]] <- temp5

write.xlsx(tables[[table_name]], file = paste("output/tables", table_name, sep = "/"),
           colNames = FALSE)



# 3 Tab A2 cause of death crosswalk ---------------------------------------

# Table name
table_name <- "TabA2.xlsx"

# Loading data
sarahLoad("cause_recode_map", folder = "data/processed")

cause_recode_map %<>%
  left_join(cause_hierarchy %>% select(ghecause, prefix, level),
            by = c("recoded_ghecause" = "ghecause"))

temp1 <- cause_recode_map %>%
  arrange(prefix, original_ghecause) %>%
  select(prefix, level, recoded_causename, original_ghecause, original_causename) %>%
  mutate(recoded_causename = ifelse(level == 3, paste0(tab, recoded_causename), recoded_causename)) %>%
  select(Level = level, "Analysis cause" = recoded_causename,
         "GHE cause code" = original_ghecause, "GHE cause" = original_causename)

tables[[table_name]] <- temp1

write.xlsx(tables[[table_name]], file = paste("output/tables", table_name, sep = "/"))



# 4 Tab A3 frontier projection method -------------------------------------

# Table name
table_name <- "TabA3.xlsx"

# Loading data
sarahLoad("frontier_projection_info", folder = "data/processed")

temp1 <- frontier_projection_info %>%
  filter(age2 %in% levels(age2)[4:8]) %>%
  mutate(causename = case_when(sex == 1 ~ paste0(causename, " (males)"),
                               sex == 2 ~ paste0(causename, " (females)"),
                               TRUE ~ causename)) %>%
  left_join(cause_hierarchy %>% select(ghecause, prefix), by = "ghecause") %>%
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
  pivot_wider(id_cols = c(causename), names_from = age2, values_from = method)

symbols_used %<>% mutate(causename = paste(symbol, "=", item)) %>% pull(causename) %>% paste(., collapse = new.line)

temp4 <- temp3 %>%
  bind_rows(data.frame(causename = symbols_used)) %>%
  dplyr::rename("Cause" = causename)

tables[[table_name]] <- temp4

write.xlsx(tables[[table_name]], file = paste("output/tables", table_name, sep = "/"),
           colNames = TRUE)
