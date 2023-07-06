
### 2.2 Frontier Harmonization

# This script harmonizes (or scales) the frontier using a level-wise approach,
# such that mortality rates of lower level causes of death sum to mortality
# rates of higher level causes of death. This script relies a function
# (`harmonize`) created in `scr/harmonize.R`.



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("cause_hierarchy", "frontier_base", "frontier_info/frontier_info_1"),
          folder = "data/processed")

# Initializing a list of figures for the scaling factor graphs
figs <- list()


# 2 Harmonizing the frontier ----------------------------------------------
# Using the `harmonize` function from `scr/harmonize.R`

# Adding cause parents and levels
data <- frontier_base %>%
  left_join(cause_hierarchy %>%
              select(ghecause, parent_ghecause, parent_causename, level),
            by = "ghecause")


# * 2.1 Level 0 -----------------------------------------------------------
# Reference level (all cause)
harmonized <- data %>%
  filter(level == 0)


# * 2.2 Level 1 -----------------------------------------------------------
# Scaling level 1 frontiers so they sum to the level 0 frontier
lvl1 <- harmonize(1, data, harmonized)$harmonized
harmonized <- bind_rows(harmonized, lvl1)

# Figure
figs[["lvl1"]] <- harmonize(1, data, harmonized)$`scaling factors`

# * 2.3 Level 2 -----------------------------------------------------------
# Scaling level 2 frontiers so they sum to level 1 frontiers
lvl2 <- harmonize(2, data, harmonized)$harmonized
harmonized <- bind_rows(harmonized, lvl2)

# Figure
figs[["lvl2"]] <- harmonize(2, data, harmonized)$`scaling factors`

# * 2.4 Level 3 -----------------------------------------------------------
# Scaling level 3 frontiers so they sum to level 2 frontiers
lvl3 <- harmonize(3, data, harmonized)$harmonized
harmonized <- bind_rows(harmonized, lvl3)

# Figure
figs[["lvl3"]] <- harmonize(3, data, harmonized)$`scaling factors`

# Saving harmonization scaling factors
saveGGplot(figs, "2-2_frontier-harmonization-scaling-factors.pdf", folder = "output/figures/process",
           width = 10, height = 6, multipage = TRUE)


# * 2.5 Checking harmonization --------------------------------------------

frontier_harmonized <- harmonized %>%
  select(year, sex, age, ghecause, causename, definition, frontier) %>%
  arrange(definition, year, age, ghecause, sex) %>%
  ungroup()

levels <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
concerns <- list()
for(i in levels){

  check <- frontier_harmonized %>%
    left_join(cause_hierarchy %>% select(ghecause, mece = !!as.name(i)),
              by = "ghecause") %>%
    filter(mece) %>%
    group_by(year, age, definition) %>%
    dplyr::summarize(lower_summed = sum(frontier), .groups = "drop") %>%
    left_join(data %>% filter(level == 0) %>%
                dplyr::select(year, age, definition, reference = frontier),
              by = c("year", "age", "definition")) %>%
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



# 3 Adding to frontier_info dataframe -------------------------------------
frontier_info_2 <- frontier_info_1 %>%
  full_join(frontier_harmonized %>% dplyr::rename(harmonized = frontier),
            by = c("year", "age", "sex", "ghecause", "causename", "definition")) %>%
  mutate(harmonize.sf = harmonized / base) %>%
  mutate(harmonize.sf = ifelse(is.nan(harmonize.sf) | is.infinite(harmonize.sf), 1, harmonize.sf)) %>%
  arrange(definition, age, ghecause, sex, year) %>%
  ungroup()

# __+ frontier_info_2 -----------------------------------------------------
sarahSave("frontier_info_2", folder = "data/processed/frontier_info")
