library(dplyr)
library(magrittr)
source("scr/colorFunct.R")

harmonize <- function(Level, lower.level.df, higher.level.df){

  return <- list()
  higher.level.df %<>%
    select(year, age, parent_ghecause = ghecause, definition, higher = frontier)

  filtered <- lower.level.df %>%
    filter(level == Level)
  summed <- filtered %>%
    group_by(year, age, parent_ghecause, definition) %>%
    dplyr::summarize(lower_summed = sum(frontier), .groups = "drop")
  compared <- summed %>%
    left_join(higher.level.df, by = c("year", "age", "parent_ghecause", "definition")) %>%
    mutate(sf = higher / lower_summed,
           sf = ifelse(is.nan(sf) | is.infinite(sf), 1, sf))

  definitions <- c("Minimum", sort(unique(summed$definition[summed$definition != "Minimum"])))
  colors <- colorFunct(n = length(definitions), names = definitions)

  return[["scaling factors"]] <- ggplot(compared %>% filter(!is.na(sf))) +
    geom_density(aes(x = sf, color = definition), size = 1) +
    scale_x_continuous("Scaling factor") + scale_y_continuous("Density") +
    scale_color_manual("", values = colors, breaks = definitions) +
    labs(title = paste("Scaling factors for level", Level, "causes of death")) +
    theme(legend.position = "bottom")

  harmonized <- filtered %>%
    left_join(compared, by = c("year", "age", "parent_ghecause", "definition")) %>%
    mutate(frontier = frontier * sf) %>%
    group_by(year, age, parent_ghecause, definition) %>%
    dplyr::mutate(lower_summed = sum(frontier), n_child = n()) %>%
    ungroup() %>%
    mutate(dif = (higher - lower_summed) / n_child) %>%
    mutate(frontier = frontier + dif,
           frontier = ifelse(frontier < 0, 0, frontier)) %>%
    dplyr::select(all_of(names(lower.level.df)))

  check <- harmonized %>%
    group_by(year, age, parent_ghecause, definition) %>%
    dplyr::summarize(lower_summed = sum(frontier), .groups = "drop") %>%
    left_join(higher.level.df, by = c("year", "age", "parent_ghecause", "definition"))

  return[["check"]] <- ggplot(check) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(aes(x = higher, y = lower_summed, color = definition)) +
    labs(title = paste("Harmonization check for level", Level, "causes of death")) +
    scale_x_continuous(paste("Mortality rates of level", Level - 1, "causes of death")) +
    scale_y_continuous(paste("Mortality rates of summed level", Level, "causes of death")) +
    scale_color_manual("", values = colors, breaks = definitions)

  Check <- check %>%
    mutate(ab.dif = round(abs(lower_summed - higher)),
           rel.dif = round(lower_summed / higher, 1)) %>%
    filter(ab.dif != 0 | rel.dif != 1)
  if(nrow(Check) > 0){
    warning("Error in harmonization")
    Check
  }

  return[["harmonized"]] <- harmonized
  return(return)

}
