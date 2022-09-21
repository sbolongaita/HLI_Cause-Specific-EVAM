library(dplyr)
library(magrittr)

scale <- function(Level, lower.level.df, higher.level.df){

  return <- list()
  higher.level.df %<>%
    select(iso3, year, sex, age, parent_ghecause = ghecause, higher = dths_rate)

  filtered <- lower.level.df %>%
    filter(level == Level)
  summed <- filtered %>%
    group_by(iso3, year, sex, age, parent_ghecause) %>%
    dplyr::summarize(lower_summed = sum(dths_rate), .groups = "drop")
  compared <- left_join(summed, higher.level.df, by = c("iso3", "year", "sex", "age", "parent_ghecause")) %>%
    mutate(sf = higher / lower_summed,
           sf = ifelse(is.nan(sf) | is.infinite(sf), 1, sf))

  return[["scaling factors"]] <- ggplot(compared %>% filter(!is.na(sf))) +
    geom_density(aes(x = sf), size = 1) +
    scale_x_continuous("Scaling factor") + scale_y_continuous("Density") +
    labs(title = paste("Scaling factors for level", Level, "causes of death"))

  scaled <- left_join(filtered, compared, by = c("iso3", "year", "sex", "age", "parent_ghecause")) %>%
    mutate(dths_rate = dths_rate * sf) %>%
    group_by(iso3, year, sex, age, parent_ghecause) %>%
    dplyr::mutate(lower_summed = sum(dths_rate), n_child = n()) %>%
    ungroup() %>%
    mutate(dif = (higher - lower_summed) / n_child) %>%
    mutate(dths_rate = dths_rate + dif,
           dths_rate = ifelse(dths_rate < 0, 0, dths_rate)) %>%
    dplyr::select(all_of(names(lower.level.df)))

  check <- scaled %>%
    group_by(iso3, year, sex, age, parent_ghecause) %>%
    dplyr::summarize(lower_summed = sum(dths_rate), .groups = "drop") %>%
    left_join(higher.level.df, by = c("iso3", "year", "sex", "age", "parent_ghecause"))

  return[["check"]] <- ggplot(check) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(aes(x = higher, y = lower_summed), pch = 21, fill = alpha("black", 0.5)) +
    labs(title = paste("Scaling check for level", Level, "causes of death")) +
    scale_x_continuous(paste("Mortality rates of level", Level - 1, "causes of death"),
                       label = labelAuto) +
    scale_y_continuous(paste("Mortality rates of summed level", Level, "causes of death"),
                       label = labelAuto)

  Check <- check %>%
    filter(lower_summed / higher < 0.99 | lower_summed / higher > 1.01)
  if(nrow(Check) > 0){
    warning("Error in scaling")
    print(Check)
  }

  return[["scaled"]] <- scaled
  return(return)

}
