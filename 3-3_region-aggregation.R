
### 3.3 Region Aggregation

# This script takes the projected and scaled (to Chang et al. 2023) country
# mortality rates and aggregates them to the regional level.



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("country_scaled", "population"), folder = "data/processed")



# 2 Back-calculating deaths from mortality rates --------------------------

# Back calculating deaths for projected years
temp1 <- left_join(country_scaled, population, by = c("iso3", "year", "age", "sex")) %>%
  mutate(dths = (dths_rate / 100000) * pop) %>%
  select(region, iso3, year, sex, age, ghecause, causename, dths, pop, dths_rate)

# Summing deaths and population by region and calculating mortality rates
region <- temp1 %>%
  group_by(region, year, sex, age, ghecause, causename) %>%
  dplyr::summarize_at(vars(dths, pop), ~ sum(.), .groups = "drop") %>%
  mutate(dths_rate = (dths / pop) * 100000) %>%
  ungroup()

# Checking scaling
reference <- region %>%
  filter(ghecause == 0) %>%
  dplyr::select(region, year, sex, age, reference = dths_rate)
levels <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
concerns <- list()
for(i in levels){

  check <- region %>%
    left_join(cause_hierarchy %>% select(ghecause, mece = !!as.name(i)),
              by = "ghecause") %>%
    filter(mece) %>%
    group_by(region, year, sex, age) %>%
    dplyr::summarize(lower_summed = sum(dths_rate), .groups = "drop") %>%
    left_join(reference, by = c("region", "year", "sex", "age")) %>%
    mutate(sf = reference / lower_summed)

  concern <- check %>%
    filter(sf < 0.99 | sf > 1.01) %>%
    arrange(desc(sf))

  if(nrow(concern) > 0){
    warning(paste("Concerning scaling factors:", names(levels[levels == i])))
    concerns[[names(levels[levels == i])]] <- concern
  }
}


# __ + region -------------------------------------------------------------
sarahSave("region", folder = "data/processed")
