
### 3.2 Country scaling

# This script takes the projected country mortality rates and scales them
# with the demographic longevity frontiers of the accompanying HLI paper,
# Chang et al. (2022), based on UN Population data.

# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("country_info", "country_projected",
            "country_projection_info/country_projection_info_1"),
          folder = "data/processed")
envelope <- read.csv("data/raw/data_mx_20perc_2040_2023may.csv", as.is = TRUE) %>%
  mutate(ghecause = 0, reference = mxn_perc * 100000,
         year = 2040) %>%
  dplyr::select(iso3, year, sex, age, ghecause, reference)



# 2 Scaling with Chang et al. envelope ------------------------------------

# Setting the Chang et al.'s longevity frontier as the all-cause frontier
# and adding cause parents and levels
country_projected %<>%
  filter(year == 2040) %>%
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
temp1 <- country_scaled %>%
  filter(causename %in% c("All causes", "Noncommunicable diseases")) %>%
  mutate(causename = ifelse(causename == "All causes", "all_mxn", "ncd_mxn"),
         dths_rate = dths_rate / 100000) %>%
  pivot_wider(id_cols = c(iso3, year, sex, age), names_from = causename, values_from = dths_rate) %>%
  mutate(ncd_frac = ncd_mxn / all_mxn)

write.csv(temp1, "output/data/for-David_2023-05-16.csv", row.names = FALSE)
