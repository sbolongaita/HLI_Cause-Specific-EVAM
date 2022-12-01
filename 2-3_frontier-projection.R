
### 2.3 Frontier projection

# This script takes the harmonized frontier mortality rates for 2010-2019
# and projects them into the future using ordinary least squares (OLS) linear
# regression, unless there are stochastic concerns or a positive trend line
# from the regression, in which case the average mortality rate from 2010-2019
# is used.


# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("frontier_base", "frontier_harmonized", "frontier_info/frontier_info_2",
            "ghe_recoded", "population"), folder = "data/processed")



# 2 Projecting the frontier to 2045 --------------------------------------

# Prepping data
frontier_harmonized %<>%
  mutate(age2 = makeMathersAgeGroup(age))

population %<>% dplyr::select(iso3, year, sex, age, pop)

ghe_recoded %<>%
  left_join(population %>% select(iso3, year, sex, age, pop),
            by = c("iso3", "year", "sex", "age")) %>%
  mutate(sex = ifelse(causename %in% sex.specific, sex, NA),
         dths_rate = dths / pop * 100000)
at_frontier <- full_join(ghe_recoded, frontier_base,
                         by = c("year", "sex", "age", "ghecause", "causename")) %>%
  filter(dths_rate <= frontier) %>%
  select(-c(pop, dths_rate, frontier))


# * 2.1 Determining projection method -------------------------------------

# Frontier mortality rates contains a 0-value
concern_A <- frontier_harmonized %>%
  group_by(age2, sex, ghecause, causename) %>%
  dplyr::summarize(frontier_min = min(frontier), .groups = "drop") %>%
  mutate(concern_A = ifelse(frontier_min == 0, "Frontier mortality rates contain 0", NA)) %>%
  filter(!is.na(concern_A)) %>%
  dplyr::select(-frontier_min) %>%
  arrange(age2, ghecause, sex) %>%
  unique()

# Average number of deaths less than 100
concern_B <- frontier_harmonized %>%
  full_join(at_frontier, by = c("year", "sex", "age", "ghecause", "causename")) %>%
  group_by(age2, sex, ghecause, causename) %>%
  dplyr::summarize(dths_mean = mean(dths), .groups = "drop") %>%
  mutate(concern_B = ifelse(dths_mean < 100, "Average number of deaths less than 100", NA)) %>%
  filter(!is.na(concern_B)) %>%
  dplyr::select(sex, age2, ghecause, causename, concern_B) %>%
  arrange(age2, ghecause, sex) %>%
  unique()

# Either concern (A) frontier mortality rates contains a 0-value or (B)
# average number deaths less than 100
concern <- full_join(concern_A, concern_B, by = c("age2", "sex", "ghecause", "causename")) %>%
  mutate(concern = case_when(!is.na(concern_A) & !is.na(concern_B) ~ paste0(concern_A, "; ", tolower(concern_B)),
                             !is.na(concern_A) ~ concern_A,
                             !is.na(concern_B) ~ concern_B,
                             TRUE ~ NA_character_)) %>%
  dplyr::select(-c(concern_A, concern_B)) %>%
  arrange(age2, ghecause, sex)

temp1 <- full_join(frontier_harmonized, concern,
                   by = c("age2", "sex", "ghecause", "causename")) %>%
  dplyr::select(year, age2, age, sex, ghecause, causename, frontier, concern)


# * 2.2 Grouping by cause, sex, and Mathers & Loncar age group ------------

# Grouping by age groups Mathers & Loncar age group (age2) and converting age
# to factor for modeling
temp2 <- temp1 %>%
  mutate(age.f = as.factor(age)) %>%
  arrange(age2, age.f, ghecause, year, sex) %>%
  group_by(age2, ghecause, causename, sex) %>%
  dplyr::mutate(group = cur_group_id()) %>% ungroup() %>%
  dplyr::select(group, sex, starts_with("age"), contains("cause"), everything()) %>%
  arrange(group)

# Noting model groups
model_groups <- temp2 %>%
  dplyr::select(group, sex, age2, ghecause, causename, concern) %>%
  unique()

# Defining model input data according to group
model_input <- temp2 %>%
  dplyr::select(group, year, age.f, frontier, concern) %>%
  filter(year >= 2010)


# * 2.3 Projecting --------------------------------------------------------

# Projecting by group
for(i in unique(model_groups$group)){

  # Running regression models
  model_data <- model_input %>% filter(group == i)

  if(any(!is.na(model_data$concern))){

    output <- model_data %>%
      group_by(group, age.f) %>%
      dplyr::summarize(projection = mean(frontier), concern = NA, .groups = "drop") %>%
      expand_grid(year = 2010:2045)

  }else{

      output <- expand_grid(model_data %>% dplyr::select(group, age.f) %>% unique(),
                            year = 2010:2045)
      if(all(model_data$age.f == 85)){
        model <- lm(log(frontier) ~ year, data = model_data)
      }else{
        model <- lm(log(frontier) ~ age.f + year, data = model_data)
      }

      if(model$coefficients["year"] < 0){
        model_prediction <- predict(model, newdata = output) %>% unname()
        output %<>% mutate(projection = exp(model_prediction),
                           concern = NA)
      }else{
        output <- model_data %>%
          group_by(group, age.f) %>%
          dplyr::summarize(projection = mean(frontier),
                           concern = "OLS linear regression indicated positive trend",
                           .groups = "drop") %>%
          expand_grid(year = 2010:2045)
      }

  }

  if(i == 1){
    Output <- output
  }else{
    Output <- bind_rows(Output, output)
  }
  if(i == max(unique(model_groups$group))){
    Output <- full_join(model_groups %>% dplyr::rename(concern_A = concern),
                        Output %>% dplyr::rename(concern_B = concern), by = "group") %>%
      mutate(age = as.numeric(as.character(age.f))) %>%
      unite(concern, c(concern_A, concern_B), sep = "; ", na.rm = TRUE, remove = TRUE) %>%
      mutate(concern = ifelse(concern == "", NA, concern)) %>%
      dplyr::select(group, year, sex, age2, age, ghecause, causename, projection, concern)
  }

}

# Joining projections with original data
frontier_projected <- full_join(frontier_harmonized, Output,
                                by = c("year", "age2", "age", "sex", "ghecause", "causename")) %>%
  select(year, age, sex, ghecause, causename, frontier, projection) %>%
  arrange(ghecause, sex, age, year)

# Indicating frontier projection info
frontier_projection_info <- Output %>%
  select(group, age2, sex, ghecause, causename, concern) %>%
  unique()

# __+ frontier_projection -------------------------------------------------
# __+ frontier_projection_info --------------------------------------------
sarahSave(c("frontier_projected", "frontier_projection_info"), folder = "data/processed")


# * 2.4 Adding to frontier_info dataframe ----------------------------------
frontier_info_3 <- frontier_info_2 %>%
  full_join(frontier_projected %>% dplyr::rename(projected = projection) %>% select(-frontier),
            by = c("year", "age", "sex", "ghecause", "causename")) %>%
  arrange(age, ghecause, year, sex)

# __+ frontier_info -------------------------------------------------------
sarahSave("frontier_info_3", folder = "data/processed/frontier_info")


# 3 GRAPHING --------------------------------------------------------------

exit()

# Formatting data for graphing
ggdata <- left_join(frontier_projected %>%
                      mutate(age2 = makeMathersAgeGroup(age),
                             age3 = makeDisplayAgeGroup(age)),
                   frontier_projection_info, by = c("sex", "age2", "ghecause", "causename")) %>%
  pivot_longer(cols = c(frontier, projection), names_to = "type", values_to = "frontier") %>%
  filter(age >= 30, is.na(sex) | sex == 2, !is.na(frontier)) %>%
  left_join(cause_hierarchy %>% select(ghecause, prefix), by = "ghecause") %>%
  mutate(age = makeAgeGroup(age),
         causename = ifelse(is.na(sex), causename, paste(causename, "(Females)")),
         concern = ifelse(is.na(concern), "OLS linear regression", "Average mortality rate"),
         type = ifelse(type == "frontier", "Observed", "Projected"),
         frontier = ifelse(frontier < 0.1, 0, frontier)) %>%
  mutate(zero = frontier == 0) %>%
  select(year, sex, age3, age2, age, prefix, ghecause, causename, type, frontier, zero, concern) %>%
  arrange(prefix, year, sex, age3, age2, age, type)

color.info <- groupColorInfo(ggdata, "age3", "age")
colors <- groupColor(color.info$n, color.info$names, color.and.fill = TRUE)
shapes <- c(21, NA)
linetypes <- c("Average mortality rate" = "dashed", "OLS linear regression" = "solid")

grobs <- list()
for(i in unique(ggdata$causename)){

  panels <- list()
  ggdata2 <- ggdata %>% filter(causename == i)
  id <- ids[which(unique(ggdata$causename) == i)]

  ylims <- ggRange(ggdata2$frontier)
  if(ylims[1] <= 0){
    ylims <- c(0.1, ylims[2])
  }

  base <- ggplot(ggdata2) +
    facet_grid(cols = vars(age3)) +
    geom_line(data = subset(ggdata2, type == "Projected" & year >= 2010),
              aes(x = year, y = frontier, color = age, linetype = concern), size = 0.5) +
    geom_point(data = subset(ggdata2, type == "Observed"),
               aes(x = year, y = frontier, color = age, fill = age, shape = zero)) +
    scale_x_continuous("", breaks = seq(2000, 2040, 10),
                       labels = c("2000", "'10", "'20", "'30", "'40")) +
    scale_color_manual("Age", values = colors$colors) +
    scale_fill_manual("Age", values = colors$fills) +
    scale_linetype_manual("Projection method", values = linetypes) +
    scale_shape_manual(values = shapes) +
    theme(axis.title.x = element_blank(),
          panel.grid.minor.y = element_blank(), legend.position = "bottom") +
    guides(color = guide_legend(title.position = "top", nrow = 3),
           fill = guide_legend(title.position = "top", nrow = 3),
           linetype = guide_legend(title.position = "top", nrow = 3),
           shape = "none")

  panels[["standard"]] <- base +
    labs(title = paste(id, i),
         subtitle = "Standard scale") +
    scale_y_continuous("Frontier mortality rate (per 100K)", limits = ylims,
                       breaks = pretty_breaks(5),
                       labels = label_number(scale_cut = cut_long_scale()))

  # Spacer
  panels[["space"]] <- ""

  # Log scale
  panels[["log"]] <- base +
    labs(subtitle = "Log scale") +
    scale_y_continuous("Frontier mortality rate (per 100K)", trans = "log", limits = ylims,
                       breaks = log.breaks, labels = log.labels)

  # Combining panels
  figure <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.1, 1),
                      common.legend = TRUE, legend = "bottom")

  grobs[[i]] <- as_grob(figure)

}

saveGGplot(grobs, "2-3_frontier-projection.pdf", folder = "output/figures",
           width = 11, height = 8.5, multipage = TRUE)

