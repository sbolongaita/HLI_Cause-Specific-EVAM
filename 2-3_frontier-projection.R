
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
sarahLoad(c("frontier_base", "frontier_harmonized",
            "frontier_info/frontier_info_2", "ghe_recoded", "population"),
          folder = "data/processed")



# 2 Projecting the frontier to 2050 --------------------------------------

# Prepping data
frontier_harmonized %<>%
  mutate(age2 = makeMathersAgeGroup(age))

population %<>% filter(region != "World") %>% select(iso3, year, sex, age, pop)

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
  group_by(age2, sex, ghecause, causename, definition) %>%
  dplyr::summarize(frontier_min = min(frontier), .groups = "drop") %>%
  mutate(concern_A = ifelse(frontier_min == 0,
                            "Frontier mortality rates contain 0", NA)) %>%
  filter(!is.na(concern_A)) %>%
  dplyr::select(-frontier_min) %>%
  arrange(definition, age2, ghecause, sex) %>%
  unique()

# Average number of deaths less than 100
concern_B <- frontier_harmonized %>%
  full_join(at_frontier,
            by = c("year", "sex", "age", "ghecause", "causename", "definition")) %>%
  group_by(age2, sex, ghecause, causename, definition) %>%
  dplyr::summarize(dths_mean = mean(dths), .groups = "drop") %>%
  mutate(concern_B = ifelse(dths_mean < 100,
                            "Average number of deaths less than 100", NA)) %>%
  filter(!is.na(concern_B)) %>%
  dplyr::select(-dths_mean) %>%
  arrange(definition, age2, ghecause, sex) %>%
  unique()

# Either concern (A) frontier mortality rates contains a 0-value or (B)
# average number deaths less than 100
concern <- full_join(concern_A, concern_B,
                     by = c("age2", "sex", "ghecause", "causename", "definition")) %>%
  mutate(concern = case_when(!is.na(concern_A) & !is.na(concern_B) ~
                               paste0(concern_A, "; ", tolower(concern_B)),
                             !is.na(concern_A) ~ concern_A,
                             !is.na(concern_B) ~ concern_B,
                             TRUE ~ NA_character_)) %>%
  dplyr::select(-c(concern_A, concern_B)) %>%
  arrange(definition, age2, ghecause, sex)

temp1 <- full_join(frontier_harmonized, concern,
                   by = c("age2", "sex", "ghecause", "causename", "definition")) %>%
  dplyr::select(year, age2, age, sex, ghecause, causename, definition, frontier, concern)


# * 2.2 Grouping by cause, sex, and Mathers & Loncar age group ------------

# Grouping by age groups Mathers & Loncar age group (age2) and converting age
# to factor for modeling
temp2 <- temp1 %>%
  mutate(age.f = as.factor(age)) %>%
  arrange(definition, age2, age.f, ghecause, year, sex) %>%
  group_by(definition, age2, ghecause, causename, sex) %>%
  dplyr::mutate(group = cur_group_id()) %>% ungroup() %>%
  dplyr::select(group, sex, starts_with("age"), contains("cause"), everything()) %>%
  arrange(group)

# Noting model groups
model_groups <- temp2 %>%
  dplyr::select(group, definition, sex, age2, ghecause, causename, concern) %>%
  unique()

# Defining model input data according to group
model_input <- temp2 %>%
  dplyr::select(group, definition, year, age.f, frontier, concern) %>%
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
      expand_grid(year = 2010:2050)

  }else{

      output <- expand_grid(model_data %>% dplyr::select(group, age.f) %>% unique(),
                            year = 2010:2050)
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
          expand_grid(year = 2010:2050)
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
      dplyr::select(group, year, sex, age2, age, ghecause, causename, definition, projection, concern)
  }

}

# Joining projections with original data
frontier_projected <- Output %>%
  select(year, age, sex, ghecause, causename, definition, frontier = projection) %>%
  arrange(definition, year, age, ghecause, sex)

# __+ frontier_projection -------------------------------------------------
sarahSave("frontier_projected", folder = "data/processed")


# * 2.4 Adding to frontier_info dataframe ----------------------------------

temp3 <- Output %>%
  select(year, age, sex, ghecause, causename, definition, projected = projection, concern)

frontier_info_3 <- frontier_info_2 %>%
  full_join(temp3, by = c("year", "sex", "age", "ghecause", "causename", "definition")) %>%
  arrange(definition, age, ghecause, sex, year)

# __+ frontier_info -------------------------------------------------------
sarahSave("frontier_info_3", folder = "data/processed/frontier_info")
