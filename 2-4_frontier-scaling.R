
### 2.4 Frontier scaling

# This script takes the harmonized and projected frontier mortality rates
# and scales them with the demographic longevity frontiers of the accompanying
# HLI paper, Chang et al. (2023), based on UN Population data.



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("cause_hierarchy", "frontier_harmonized", "frontier_projected", "frontier_info/frontier_info_3"),
          folder = "data/processed")
envelope <- read.csv("data/input/chang_frontier.csv", as.is = TRUE) %>%
  mutate(reference = mxn * 100000) %>%
  select(year, age, reference)

# Initializing a list of figures for the scaling factor graphs
figs <- list()


# 2 Scaling with Chang et al. envelope ------------------------------------

# Setting the Chang et al.'s longevity frontier as the all-cause frontier
# and adding cause parents and levels
temp1 <- full_join(frontier_harmonized %>% rename(harmonized = frontier),
                   frontier_projected %>% rename(projected = frontier),
                   by = c("year", "sex", "age", "ghecause", "causename", "definition")) %>%
  arrange(definition, age, ghecause, sex, year) %>%
  mutate(frontier = ifelse(!is.na(harmonized), harmonized, projected)) %>%
  left_join(envelope %>% mutate(ghecause = 0, causename = "All causes"),
            by = c("year", "age", "ghecause", "causename"))

chang_scaling_factors <- temp1 %>%
  filter(!is.na(reference)) %>%
  mutate(sf = reference / frontier) %>%
  ungroup()

# __+ chang_scaling_factors -------------------------------------------
sarahSave("chang_scaling_factors", folder = "data/processed")

data <- temp1 %>%
  left_join(cause_hierarchy %>%
              select(ghecause, parent_ghecause, parent_causename, level),
            by = "ghecause") %>%
  mutate(frontier = ifelse(!is.na(reference), reference, frontier)) %>%
  select(-c(harmonized, projected, reference)) %>%
  arrange(definition, year, age, ghecause, sex)


# * 2.1 L0 ----------------------------------------------------------------
# Reference level (Chang et al. longevity frontier)
scaled <- data %>%
  filter(level == 0)


# * 2.2 L1 ----------------------------------------------------------------
# Scaling level 1 frontiers so they sum to the level 0 frontier
lvl1 <- harmonize(1, data, scaled)$harmonized
scaled <- bind_rows(scaled, lvl1)

# Figure
figs[["lvl1"]] <- harmonize(1, data, scaled)$`scaling factors`

# * 2.3 L2 ----------------------------------------------------------------
# Scaling level 2 frontiers so they sum to level 1 frontiers
lvl2 <- harmonize(2, data, scaled)$harmonized
scaled <- bind_rows(scaled, lvl2)

# Figure
figs[["lvl2"]] <- harmonize(2, data, scaled)$`scaling factors`

# * 2.4 L3 ----------------------------------------------------------------
# Scaling level 3 frontiers so they sum to level 2 frontiers
lvl3 <- harmonize(3, data, scaled)$harmonized
scaled <- bind_rows(scaled, lvl3)

# Figure
figs[["lvl3"]] <- harmonize(3, data, scaled)$`scaling factors`

# Saving scaling scaling factors
saveGGplot(figs, "2-4_frontier-scaling-scaling-factors.pdf", folder = "output/figures/process",
           width = 10, height = 6, multipage = TRUE)

# * 2.5 Arranging data ----------------------------------------------------
frontier_scaled <- scaled %>%
  select(year, age, sex, ghecause, causename, definition, frontier) %>%
  arrange(definition, year, age, ghecause, sex) %>%
  ungroup()


# * 2.6 Checking scaling --------------------------------------------------
# Checking harmonization
levels <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
concerns <- list()
for(i in levels){

  check <- frontier_scaled %>%
    left_join(cause_hierarchy %>% select(ghecause, mece = !!as.name(i)),
              by  = "ghecause") %>%
    filter(mece) %>%
    group_by(year, age, definition) %>%
    dplyr::summarize(lower_summed = sum(frontier), .groups = "drop") %>%
    left_join(envelope, by = c("year", "age")) %>%
    mutate(sf = reference / lower_summed)

  concern <- check %>%
    filter(sf < 0.99 | sf > 1.01) %>%
    arrange(desc(sf))

  if(nrow(concern) > 0){
    warning(paste("Concerning scaling factors:", names(levels[levels == i])))
    concerns[[names(levels[levels == i])]] <- concern
  }
}

# __+ frontier_scaled -----------------------------------------------------
sarahSave("frontier_scaled", folder = "data/processed")


# * 2.7 Adding to frontier_info dataframe ----------------------------------
frontier_info_4 <- frontier_info_3 %>%
  full_join(frontier_scaled %>% dplyr::rename(scaled = frontier),
            by = c("year", "age", "sex", "ghecause", "causename", "definition")) %>%
  mutate(scale.sf = ifelse(!is.na(harmonized), harmonized, projected)) %>%
  mutate(scale.sf = scaled / scale.sf) %>%
  mutate(scale.sf = ifelse(is.nan(scale.sf) | is.infinite(scale.sf), 1, scale.sf)) %>%
  arrange(definition, age, ghecause, sex, year) %>%
  ungroup()

# __+ frontier_info -------------------------------------------------------
sarahSave("frontier_info_4", folder = "data/processed/frontier_info")
