
### 3.2 Country scaling

# This script takes the projected country mortality rates and scales them
# with the demographic longevity frontiers of the accompanying HLI paper,
# Chang et al. (2023).



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("country_info", "country_projected",
            "country_projection_info/country_projection_info_1"),
          folder = "data/processed")
envelope <- read.csv("data/input/chang_country.csv", as.is = TRUE) %>%
  filter(year >= 2000) %>%
  mutate(ghecause = 0, reference = mxn * 100000) %>%
  dplyr::select(iso3, year, sex, age, ghecause, reference)



# 2 Scaling with Chang et al. envelope ------------------------------------

# Setting the Chang et al.'s longevity frontier as the all-cause frontier
# and adding cause parents and levels
country_projected %<>%
  mutate(dths_rate = ifelse(is.na(dths_rate), projection, dths_rate)) %>%
  select(-projection)

data <- left_join(country_projected, envelope,
                  by = c("iso3", "year", "sex", "age", "ghecause")) %>%
  left_join(cause_hierarchy %>%
              select(ghecause, parent_ghecause, parent_causename, level),
            by = "ghecause") %>%
  mutate(dths_rate = ifelse(!is.na(reference), reference, dths_rate)) %>%
  select(-reference) %>%
  arrange(iso3, year, age, ghecause, sex)


# * 2.1 Level 0 -----------------------------------------------------------
# Reference level (Chang et al. longevity frontier)
scaled <- data %>%
  filter(level == 0)


# * 2.2 Level 1 -----------------------------------------------------------
# Scaling level 1 frontiers so they sum to the level 0 frontier
lvl1 <- scale(1, data, scaled)$scaled
scaled <- bind_rows(scaled, lvl1)


# * 2.3 Level 2 -----------------------------------------------------------
# Scaling level 2 frontiers so they sum to level 1 frontiers
lvl2 <- scale(2, data, scaled)$scaled
scaled <- bind_rows(scaled, lvl2)


# * 2.4 Level 3 -----------------------------------------------------------
# Scaling level 3 frontiers so they sum to level 2 frontiers
lvl3 <- scale(3, data, scaled)$scaled
scaled <- bind_rows(scaled, lvl3)


# * 2.5 Arranging data ----------------------------------------------------
country_scaled <- scaled %>%
  select(iso3, year, sex, age, ghecause, causename, dths_rate) %>%
  arrange(iso3, year, age, ghecause, sex) %>%
  ungroup()


# * 2.6 Checking scaling --------------------------------------------------

# Checking scaling
levels <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
concerns <- list()
for(i in levels){

  check <- country_scaled %>%
    left_join(cause_hierarchy %>% select(ghecause, mece = !!as.name(i)),
              by  = "ghecause") %>%
    filter(mece) %>%
    group_by(iso3, year, sex, age) %>%
    dplyr::summarize(lower_summed = sum(dths_rate), .groups = "drop") %>%
    left_join(envelope, by = c("iso3", "year", "sex", "age")) %>%
    mutate(sf = reference / lower_summed)

  concern <- check %>%
    filter(sf < 0.99 | sf > 1.01) %>%
    arrange(desc(sf))

  if(nrow(concern) > 0){
    warning(paste("Concerning scaling factors:", names(levels[levels == i])))
    concerns[[names(levels[levels == i])]] <- concern
  }
}

# __+ country_scaled -----------------------------------------------------
sarahSave("country_scaled", folder = "data/processed")


# * 2.7 Adding to country_projection_info dataframe -----------------------
country_projection_info_2 <- country_projection_info_1 %>%
  full_join(country_scaled %>% dplyr::rename(scaled = dths_rate),
            by = c("year", "iso3", "sex", "age", "ghecause", "causename")) %>%
  arrange(iso3, age, ghecause, year, sex) %>%
  ungroup()

# __+ country_projection_info_2 --------------------------------------------
sarahSave("country_projection_info_2",
          folder = "data/processed/country_projection_info")
