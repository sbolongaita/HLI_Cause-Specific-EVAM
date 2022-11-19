
### 3.1 Country projection



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("country_info", "ghe_recoded", "population"),
          folder = "data/processed")



# 2 Projecting country mortality ------------------------------------------

# Calculating age-cause-sex-year-specific mortality rates for analysis-eligible
# countries and creating age groups according to Mathers & Loncar (https://doi.org/10.1371/journal.pmed.0030442)
temp1 <- ghe_recoded %>%
  left_join(population %>% select(iso3, year, sex, age, pop),
            by = c("iso3", "year", "sex", "age")) %>%
  mutate(age2 = makeMathersAgeGroup(age),
         dths_rate = dths / pop * 100000) %>%
  select(iso3, year, age2, age, everything())


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
  dplyr::select(-c(concern_A, concern_B)) %>%
  arrange(iso3, age2, ghecause, sex)

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

# __+ country_projected ---------------------------------------------------
sarahSave("country_projected", folder = "data/processed")


# 3 Creating a country projection info dataframe --------------------------
# Documenting information about the country projections

temp1 <- country_projected %>%
  rename(base = dths_rate, projected = projection) %>%
  mutate(age2 = makeMathersAgeGroup(age))

temp2 <- full_join(model_groups %>% dplyr::select(-c(concern, age.f)) %>% unique(), Output_concern, by = "group") %>%
  mutate(projection.concern = ifelse(is.na(concern), concern2, concern)) %>%
  mutate(projection.method = ifelse(is.na(projection.concern), "OLS linear regression", "Average mortality rate")) %>%
  dplyr::select(iso3, sex, age2, ghecause, causename, projection.method, projection.concern)

country_projection_info_1 <- full_join(temp1, temp2, by = c("iso3", "sex", "age2", "ghecause", "causename")) %>%
  select(year, iso3, sex, age2, age, ghecause, causename, base, projected, projection.method, projection.concern) %>%
  arrange(iso3, ghecause, sex, age2, age, year)

# __+ country_projection_info_1 -------------------------------------------
if(!dir.exists("data/processed/country_projection_info")){
  dir.create("data/processed/country_projection_info")
}
sarahSave("country_projection_info_1", folder = "data/processed/country_projection_info")


# 4 Graphing --------------------------------------------------------------

# Formatting data for graphing
iso3s <- population %>%
  filter(year == max(year), iso3 %notin% c("IND", "CHN")) %>%
  group_by(iso3) %>%
  summarize(pop = sum(pop)) %>%
  arrange(desc(pop)) %>%
  slice_head(n = 5) %>%
  pull(iso3)

ggdata <- country_projection_info_1 %>%
  filter(iso3 %in% iso3s) %>%
  mutate(age3 = makeDisplayAgeGroup(age)) %>%
  pivot_longer(cols = c(base, projected), names_to = "type", values_to = "dths_rate") %>%
  filter(age >= 30, !is.na(dths_rate)) %>%
  left_join(cause_hierarchy %>% select(ghecause, prefix), by = "ghecause") %>%
  mutate(age = makeAgeGroup(age),
         sex = ifelse(sex == 1, "Males", "Females"),
         type = ifelse(type == "base", "Observed", "Projected"),
         dths_rate = ifelse(dths_rate < 0.1, 0, dths_rate)) %>%
  mutate(zero = dths_rate == 0) %>%
  select(iso3, year, sex, age3, age2, age, prefix, ghecause, causename, type, dths_rate, zero, projection.method) %>%
  arrange(iso3, prefix, year, sex, age3, age2, age, type)

color.info <- groupColorInfo(ggdata, "age3", "age")
colors <- groupColor(color.info$n, color.info$names, color.and.fill = TRUE)
shapes <- c(21, NA)
linetypes <- c("Average mortality rate" = "dashed", "OLS linear regression" = "solid")

for(j in unique(ggdata$iso3)){
  grobs <- list()
  for(i in unique(ggdata$causename)){

    panels <- list()
    ggdata2 <- ggdata %>% filter(iso3 == j, causename == i)

    country <- country_info$country[country_info$iso3 == j]
    id <- ids[which(unique(ggdata$causename) == i)]

    ylims <- ggRange(ggdata2$dths_rate)
    if(ylims[1] <= 0){
      ylims <- c(0.1, ylims[2])
    }

    base <- ggplot(ggdata2) +
      facet_grid(cols = vars(age3), rows = vars(sex)) +
      geom_line(data = subset(ggdata2, type == "Projected" & year >= 2010),
                aes(x = year, y = dths_rate, color = age, linetype = projection.method), size = 0.5) +
      geom_point(data = subset(ggdata2, type == "Observed"),
                 aes(x = year, y = dths_rate, color = age, fill = age, shape = zero)) +
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
      labs(title = paste(id, paste0(country, ": ", i)),
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

  filename <- gsub("ISO", j, "3-1_country-projection_ISO.pdf")
  saveGGplot(grobs, filename, folder = "output/figures",
             width = 11, height = 12, multipage = TRUE)

}
