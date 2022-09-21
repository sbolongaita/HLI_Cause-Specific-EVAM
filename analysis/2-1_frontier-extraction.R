
### Healthy Longevity Initiative
### 2-1 Frontier extraction



# 1 ENVIRONMENT -----------------------------------------------------------

applyEnv()

# Loading data
sarahLoad(c("country_info", "ghe_recoded", "population"), folder = "data/processed")



# 2 EXTRACTING THE FRONTIER -----------------------------------------------

# Calculating age-cause-sex-year-specific mortality rates for frontier-eligible
# countries
ghe_recoded %<>%
  filter(iso3 %in% country_info$iso3[country_info$frontier_eligible]) %>%
  left_join(population %>% select(iso3 = iso3.region, year, sex, age, pop),
            by = c("iso3", "year", "sex", "age")) %>%
  mutate(dths_rate = dths / pop * 100000)

# Extracting the 10th percentile
frontier_base <- ghe_recoded %>%
  mutate(sex = ifelse(causename %in% sex.specific, sex, NA)) %>%
  group_by(year, age, ghecause, causename, sex) %>%
  dplyr::summarize(frontier = unname(quantile(dths_rate, probs = 0.1, type = 3)), .groups = "drop") %>%
  dplyr::select(year, sex, age, ghecause, causename, everything()) %>%
  arrange(year, age, ghecause, sex)

# __+ frontier_base -------------------------------------------------------
sarahSave("frontier_base", folder = "data/processed")



# 3 HARMONIZING THE FRONTIER ----------------------------------------------
# Uses the `harmonize` function from `util/harmonize.R`

# Adding cause parents and levels
data <- frontier_base %>%
  left_join(cause_hierarchy %>% select(ghecause, parent_ghecause, parent_causename, level),
            by = "ghecause")


# * 3.1 Level 0 -----------------------------------------------------------
# Reference level (Chang et al. longevity frontier)
harmonized <- data %>%
  filter(level == 0)


# * 3.2 Level 1 -----------------------------------------------------------
# Scaling level 1 frontiers so they sum to the level 0 frontier
lvl1 <- harmonize(1, data, harmonized)$harmonized
harmonized <- bind_rows(harmonized, lvl1)


# * 3.3 Level 2 -----------------------------------------------------------
# Scaling level 2 frontiers so they sum to level 1 frontiers
lvl2 <- harmonize(2, data, harmonized)$harmonized
harmonized <- bind_rows(harmonized, lvl2)


# * 3.4 Level 3 -----------------------------------------------------------
# Scaling level 3 frontiers so they sum to level 2 frontiers
lvl3 <- harmonize(3, data, harmonized)$harmonized
harmonized <- bind_rows(harmonized, lvl3)


# * 3.5 Arranging data ----------------------------------------------------
frontier_harmonized <- harmonized %>%
  select(year, sex, age, ghecause, causename, frontier)


# * 3.6 Checking harmonization --------------------------------------------
# Checking harmonization
levels <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
concerns <- list()
for(i in levels){

  check <- frontier_harmonized %>%
    left_join(cause_hierarchy %>% select(ghecause, mece = !!as.name(i)), by = "ghecause") %>%
    filter(mece) %>%
    group_by(year, age) %>%
    dplyr::summarize(lower_summed = sum(frontier), .groups = "drop") %>%
    left_join(data %>% filter(level == 0) %>% dplyr::select(year, age, reference = frontier),
              by = c("year", "age")) %>%
    mutate(sf = reference / lower_summed)

  concern <- check %>%
    filter(sf < 0.99 | sf > 1.01) %>%
    arrange(desc(sf))

  if(nrow(concern) > 0){
    warning(paste("Concerning scaling factors:", names(levels[levels == i])))
    concerns[[names(levels[levels == i])]] <- concern
  }
}

# __+ frontier_harmonized -------------------------------------------------
sarahSave("frontier_harmonized", folder = "data/processed")



# 4 FRONTIER INFO ---------------------------------------------------------
# Documenting information about the frontier, including the age-country-sex
# groups that were at or below a given frontier for a given year, as well as
# scaling factors for the frontiers

frontier_extraction_info <- ghe_recoded %>%
  dplyr::mutate(sex_match = ifelse(causename %in% sex.specific, sex, NA)) %>%
  left_join(frontier_base %>% dplyr::rename(sex_match = sex, frontier_base = frontier),
            by = c("year", "age", "ghecause", "causename", "sex_match")) %>%
  filter(dths_rate <= frontier_base) %>%
  left_join(frontier_harmonized %>% dplyr::rename(sex_match = sex),
            by = c("year", "sex_match", "age", "ghecause", "causename")) %>%
  dplyr::select(year, sex_match, age, ghecause, causename, frontier_base, frontier_harmonized = frontier, iso3, sex, dths_rate) %>%
  arrange(year, age, ghecause, sex_match, dths_rate)

# __+ frontier_extraction_info ----------------------------------------------
sarahSave("frontier_extraction_info", folder = "data/processed")
