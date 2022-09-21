
### Healthy Longevity Initiative
### 3-1 Country projection



# 1 ENVIRONMENT -----------------------------------------------------------

applyEnv()

# Loading data
sarahLoad(c("country_info", "ghe_recoded", "population"), folder = "data/processed")



# 2 PROJECTING ------------------------------------------------------------

# Calculating age-cause-sex-year-specific mortality rates for analysis-eligible
# countries and creating age groups according to Mathers & Loncar (https://doi.org/10.1371/journal.pmed.0030442)
temp1 <- ghe_recoded %>%
  left_join(population %>% select(iso3 = iso3.region, year, sex, age, pop),
            by = c("iso3", "year", "sex", "age")) %>%
  mutate(age2 = makeMathersAgeGroup(age),
         dths_rate = dths / pop * 100000) %>%
  select(iso3, year, sex, age2, age, everything())


# * 2.1 Determining projection method -------------------------------------

# Mortality rates contains a 0-value
concern_A <- temp1 %>%
  group_by(iso3, age2, sex, ghecause, causename) %>%
  dplyr::summarize(dths_rate_min = min(dths_rate), .groups = "drop") %>%
  mutate(concern_A = ifelse(dths_rate_min == 0, "Mortality rates contain 0", NA)) %>%
  filter(!is.na(concern_A)) %>%
  dplyr::select(-dths_rate_min)

# Average number of deaths less than 100
concern_B <- temp1 %>%
  group_by(iso3, age2, sex, ghecause, causename) %>%
  dplyr::summarize(dths_mean = mean(dths), .groups = "drop") %>%
  mutate(concern_B = ifelse(dths_mean < 100, "Average number of deaths less than 100", NA)) %>%
  filter(!is.na(concern_B)) %>%
  dplyr::select(-dths_mean)

# Either concern (A) mortality rates contains a 0-value or (B) average number deaths
# less than 100
concern <- full_join(concern_A, concern_B, by = c("iso3", "sex", "age2", "ghecause", "causename")) %>%
  mutate(concern = case_when(!is.na(concern_A) & !is.na(concern_B) ~ paste0(concern_A, "; ", tolower(concern_B)),
                             !is.na(concern_A) ~ concern_A,
                             !is.na(concern_B) ~ concern_B,
                             TRUE ~ NA_character_)) %>%
  dplyr::select(-c(concern_A, concern_B))

temp2 <- full_join(temp1, concern, by = c("iso3", "sex", "age2", "ghecause", "causename")) %>%
  dplyr::select(iso3, year, sex, age2, age, ghecause, causename, dths_rate, concern)


# * 2.2 Grouping by cause, sex, and Mathers & Loncar age group ------------

# Grouping by age groups Mathers & Loncar age group (age2) and converting age
# to factor for modeling
temp3 <- temp2 %>%
  mutate(age.f = as.factor(age)) %>%
  arrange(iso3, sex, age2, age, ghecause) %>%
  group_by(iso3, age2, sex, ghecause, causename) %>%
  dplyr::mutate(group = cur_group_id()) %>%
  dplyr::select(group, iso3, year, sex, starts_with("age"), contains("cause"), everything()) %>%
  ungroup()

# Noting model groups
model_groups <- temp3 %>%
  dplyr::select(group, iso3, age2, age.f, sex, ghecause, causename, concern) %>%
  unique()

# Defining model input data according to group
model_input <- temp3 %>%
  dplyr::select(group, year, age.f, dths_rate, concern) %>%
  filter(year >= 2010)


# * 2.3 Projecting --------------------------------------------------------

# Setting up progress bar
n_int = 20
progress_int <- round(seq(from = 1, to = max(model_groups$group), length.out = n_int))
progress_bar <- txtProgressBar(min = 0, max = max(progress_int), style = 3, char = "=", width = 100)

for(i in unique(model_groups$group)){

  # Running regression models
  model_data <- model_input %>% filter(group == i) %>% dplyr::select(-concern)
  output_concern <- model_input %>% filter(group == i) %>% dplyr::select(group, concern) %>% unique()

  if(any(!is.na(output_concern$concern))){

    output <- model_data %>%
      group_by(group, age.f) %>%
      dplyr::summarize(projection = mean(dths_rate), .groups = "drop") %>%
      expand_grid(year = 2010:2045) %>%
      dplyr::select(group, age.f, year, projection)
    output_concern$concern2 <- NA

  }else{

      output <- expand_grid(model_data %>% dplyr::select(group, age.f) %>% unique(),
                            year = 2010:2045)
      if(all(model_data$age.f == 85)){
        model <- lm(log(dths_rate) ~ year, data = model_data)
      }else{
        model <- lm(log(dths_rate) ~ age.f + year, data = model_data)
      }

      if(model$coefficients["year"] < 0){
        model_prediction <- predict(model, newdata = output) %>% unname()
        output %<>% mutate(projection = exp(model_prediction))
        output_concern$concern2 <- NA
      }else{
        output <- model_data %>%
          group_by(group, age.f) %>%
          dplyr::summarize(projection = mean(dths_rate), .groups = "drop") %>%
          expand_grid(year = 2010:2045) %>%
          dplyr::select(group, age.f, year, projection)
        output_concern$concern2 <- "OLS linear regression indicated positive trend"
      }

  }

  if(i == 1){
    Output <- output
    Output_concern <- output_concern
  }else{
    Output <- bind_rows(Output, output)
    Output_concern <- bind_rows(Output_concern, output_concern)
  }

  # Progress bar
  setTxtProgressBar(progress_bar, i)

  if(i == max(model_groups$group)){
    Output <- full_join(model_groups, Output, by = c("group", "age.f")) %>%
      dplyr::select(group, iso3, year, age2, age.f, sex, ghecause, causename, projection)
    }

}

# Joining projections with original data
country_projected <- full_join(temp3 %>% dplyr::select(-c(concern, age)), Output,
                               by = c("group", "iso3", "year", "age2", "age.f", "sex", "ghecause", "causename")) %>%
  mutate(age = as.numeric(as.character(age.f)),
         projection = projection) %>%
  dplyr::select(iso3, year, age, sex, ghecause, causename, dths_rate, projection) %>%
  arrange(iso3, age, sex, ghecause, year)

# Indicating frontier projection info
country_projection_info <- full_join(model_groups %>% dplyr::select(-c(concern, age.f)) %>% unique(),
                                     Output_concern, by = "group") %>%
  mutate(concern = ifelse(is.na(concern), concern2, concern)) %>%
  dplyr::select(iso3, sex, age2, ghecause, causename, concern)

# __+ country_projected ---------------------------------------------------
# __+ country_projection_info ---------------------------------------------
sarahSave(c("country_projected", "country_projection_info"), folder = "data/processed")
