
### Healthy Longevity Initiative
### 210 - FRONTIER ANALYSIS

# This script takes the recoded cause of death data for frontier-eligible
# countries, calculates age-cause-specific mortality rates, and the extracts
# the frontier using a 10th percentile definition. It then harmonizes (or
# scales) the frontier using a level-wise approach.

# Returns:
# • output_data/frontier_base.R (R dataset)
# • output_data/frontier_harmonized.Rda (R dataset)
# • output_data/frontier_analysis_info.R (R dataset)



# 1 ENVIRONMENT -----------------------------------------------------------

# Clearing and loading environment
source("000_environment.R")
scriptName <- "210-frontier_analysis.R"

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


# * 2.1 Examining two definitions of the frontier -------------------------

# Minimum
min <- ghe_recoded %>%
  mutate(sex = ifelse(causename %in% sex.specific, sex, NA)) %>%
  group_by(year, age, ghecause, causename, sex) %>%
  dplyr::summarize(min = min(dths_rate), .groups = "drop") %>%
  dplyr::select(year, sex, age, ghecause, causename, everything()) %>%
  arrange(year, age, ghecause, sex)

# 10th percentile
p10 <- ghe_recoded %>%
  mutate(sex = ifelse(causename %in% sex.specific, sex, NA)) %>%
  group_by(year, age, ghecause, causename, sex) %>%
  dplyr::summarize(p10 = unname(quantile(dths_rate, probs = 0.1, type = 3)), .groups = "drop") %>%
  dplyr::select(year, sex, age, ghecause, causename, everything()) %>%
  arrange(year, age, ghecause, sex)

# Combining for comparison
frontier_comparison <- full_join(min, p10, by = c("year", "sex", "age", "ghecause", "causename"))

# __+ frontier_comparison -------------------------------------------------
sarahSave("frontier_comparison", folder = "data/processed")


# * 2.2 Selecting 10th percentile -----------------------------------------
frontier_base <- p10 %>%
  dplyr::rename(frontier = p10)

# __+ frontier_base -------------------------------------------------------
sarahSave("frontier_base", folder = "data/processed")
sarahSave("frontier_base", type = ".csv", folder = "output/data")



# 3 HARMONIZING THE FRONTIER ----------------------------------------------

# Creating a function for harmonizing
harmonize <- function(Level, lower.level.df, higher.level.df){

  return <- list()
  higher.level.df <- higher.level.df %>%
    select(year, age, parent_ghecause = ghecause, higher = frontier)

  filtered <- lower.level.df %>%
    filter(level == Level)
  summed <- filtered %>%
    group_by(year, age, parent_ghecause) %>%
    dplyr::summarize(lower_summed = sum(frontier), .groups = "drop")
  compared <- left_join(summed, higher.level.df, by = c("year", "age", "parent_ghecause")) %>%
    mutate(sf = higher / lower_summed,
           sf = ifelse(is.nan(sf) | is.infinite(sf), 1, sf))

  return[["scaling factors"]] <- ggplot(compared %>% filter(!is.na(sf))) +
    geom_density(aes(x = sf), size = 1) +
    scale_x_continuous("Scaling factor") + scale_y_continuous("Density") +
    labs(title = paste("Scaling factors for level", Level, "causes of death"))

  harmonized <- left_join(filtered, compared, by = c("year", "age", "parent_ghecause")) %>%
    mutate(frontier = frontier * sf) %>%
    group_by(year, age, parent_ghecause) %>%
    dplyr::mutate(lower_summed = sum(frontier), n_child = n()) %>%
    ungroup() %>%
    mutate(dif = (higher - lower_summed) / n_child) %>%
    mutate(frontier = frontier + dif,
           frontier = ifelse(frontier < 0, 0, frontier)) %>%
    dplyr::select(all_of(names(lower.level.df)))

  check <- harmonized %>%
    group_by(year, age, parent_ghecause) %>%
    dplyr::summarize(lower_summed = sum(frontier), .groups = "drop") %>%
    left_join(higher.level.df, by = c("year", "age", "parent_ghecause"))

  return[["check"]] <- ggplot(check) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(aes(x = higher, y = lower_summed), pch = 21, fill = alpha("black", 0.5)) +
    labs(title = paste("Harmonization check for level", Level, "causes of death")) +
    scale_x_continuous(paste("Mortality rates of level", Level - 1, "causes of death")) +
    scale_y_continuous(paste("Mortality rates of summed level", Level, "causes of death"))

  Check <- check %>%
    filter(round(lower_summed) != round(higher))
  if(nrow(Check) > 0){
    warning("Error in harmonization")
    Check
  }

  return[["harmonized"]] <- harmonized
  return(return)

}

# __+ harmonize -----------------------------------------------------------
sarahSave("harmonize", folder = "data/processed")


# Adding cause parents and levels
data <- frontier_base %>%
  mutate(parent_ghecause = getCauseInfo(ghecause, return = "parent_ghecause"),
         parent_causename = getCauseInfo(ghecause, return = "parent_causename"),
         level = getCauseInfo(ghecause, return = "level"))


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
Order <- order[order %in% names(harmonized)]
Sort <- sort[sort %in% names(harmonized)]
frontier_harmonized <- harmonized %>%
  dplyr::select(all_of(Order), frontier) %>%
  dplyr::arrange(!!!rlang::parse_exprs(Sort))


# * 3.6 Checking harmonization --------------------------------------------
# Checking harmonization
levels <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
concerns <- list()
for(i in levels){

  check <- frontier_harmonized %>%
    mutate(mece = getCauseInfo(ghecause, return = i)) %>%
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
sarahSave("frontier_harmonized", type = "both")



# 4 FRONTIER INFO ---------------------------------------------------------
# Documenting information about the frontier, including the age-country-sex
# groups that were at or below a given frontier for a given year, as well as
# scaling factors for the frontiers

frontier_analysis_info <- ghe_recoded %>%
  dplyr::mutate(sex_match = ifelse(causename %in% sex.specific.causename, sex, NA)) %>%
  left_join(frontier_base %>% dplyr::rename(sex_match = sex, frontier_base = frontier),
            by = c("year", "age", "ghecause", "causename", "sex_match")) %>%
  filter(dths_rate <= frontier_base) %>%
  left_join(frontier_harmonized %>% dplyr::rename(sex_match = sex),
            by = c("year", "sex_match", "age", "ghecause", "causename")) %>%
  dplyr::select(year, sex_match, age, ghecause, causename, frontier_base, frontier_harmonized = frontier, iso3, sex, dths_rate) %>%
  arrange(year, age, ghecause, sex_match, dths_rate)

# __+ frontier_analysis_info ----------------------------------------------
sarahSave("frontier_analysis_info", type = "both")



# 5 END -------------------------------------------------------------------

# Tidying environment and notifying the end of the script
notifyScript(); tidy()
