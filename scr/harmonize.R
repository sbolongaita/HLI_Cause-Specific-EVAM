library(dplyr)
library(magrittr)

harmonize <- function(Level, lower.level.df, higher.level.df){

  return <- list()
  higher.level.df %<>%
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
    filter(signif(lower_summed, 1) != signif(higher, 1))
  if(nrow(Check) > 0){
    warning("Error in harmonization")
    Check
  }

  return[["harmonized"]] <- harmonized
  return(return)

}
