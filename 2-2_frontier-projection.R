
### Healthy Longevity Initiative
### 2-2 Frontier projection



# 1 ENVIRONMENT -----------------------------------------------------------

applyEnv()

# Loading data
sarahLoad(c("country_info", "frontier_harmonized", "frontier_extraction_info", "population"),
          folder = "data/processed")
population %<>% dplyr::select(iso3 = iso3.region, year, sex, age, pop)



# 2 PROJECTING ------------------------------------------------------------

# Prepping data
frontier_harmonized %<>%
  mutate(age2 = makeMathersAgeGroup(age))

frontier_analysis_info %<>%
  filter(frontier_base == dths_rate) %>%
  left_join(population, by = c("iso3", "year", "age", "sex")) %>%
  mutate(dths = dths_rate / 100000 * pop,
         age2 = makeMathersAgeGroup(age))


# * 2.1 Determining projection method -------------------------------------

# Frontier mortality rates contains a 0-value
concern_A <- frontier_harmonized %>%
  group_by(sex, age2, ghecause, causename) %>%
  dplyr::summarize(frontier_min = min(frontier), .groups = "drop") %>%
  mutate(concern_A = ifelse(frontier_min == 0, "Frontier mortality rates contain 0", NA)) %>%
  filter(!is.na(concern_A)) %>%
  dplyr::select(-frontier_min) %>%
  arrange(age2, ghecause) %>%
  unique()

# Average number of deaths less than 100
concern_B <- frontier_analysis_info %>%
  group_by(sex_match, age2, ghecause, causename) %>%
  dplyr::summarize(dths_mean = mean(dths), .groups = "drop") %>%
  mutate(concern_B = ifelse(dths_mean < 100, "Average number of deaths less than 100", NA)) %>%
  filter(!is.na(concern_B)) %>%
  dplyr::select(sex = sex_match, age2, ghecause, causename, concern_B) %>%
  arrange(age2, ghecause) %>%
  unique()

# Either concern (A) frontier mortality rates contains a 0-value or (B)
# average number deaths less than 100
concern <- full_join(concern_A, concern_B, by = c("sex", "age2", "ghecause", "causename")) %>%
  mutate(concern = case_when(!is.na(concern_A) & !is.na(concern_B) ~ paste0(concern_A, "; ", tolower(concern_B)),
                             !is.na(concern_A) ~ concern_A,
                             !is.na(concern_B) ~ concern_B,
                             TRUE ~ NA_character_)) %>%
  dplyr::select(-c(concern_A, concern_B))

temp1 <- full_join(frontier_harmonized, concern, by = c("sex", "age2", "ghecause", "causename")) %>%
  dplyr::select(year, sex, age2, age, ghecause, causename, frontier, concern)


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
                                by = c("year", "sex", "age2", "age", "ghecause", "causename")) %>%
  select(year, sex, age, ghecause, causename, frontier, projection) %>%
  arrange(ghecause, sex, age, year)

# Indicating frontier projection info
frontier_projection_info <- Output %>%
  select(group, sex, age2, ghecause, causename, concern) %>%
  unique()

# __+ frontier_projection -------------------------------------------------
# __+ frontier_projection_info --------------------------------------------
sarahSave(c("frontier_projected", "frontier_projection_info"), folder = "data/processed")


#
# # 3 GRAPHING --------------------------------------------------------------
#
# # Creating figures objects
# folder <- makeFolder(figures)
#
# # Formatting data for graphing
# temp1 <- left_join(frontier_projected %>% mutate(age2 = makeMathersAgeGroup(age)),
#                    frontier_projection_info, by = c("sex", "age2", "ghecause", "causename")) %>%
#   pivot_longer(cols = c(frontier, projection), names_to = "type", values_to = "nmx") %>%
#   filter(is.na(sex) | sex == 2, !is.na(nmx)) %>%
#   mutate(age = makeAgeGroup(age),
#          causename = ifelse(is.na(sex), causename, paste(causename, "(Females)")),
#          concern = ifelse(is.na(concern), "OLS linear regression", "Average mortality rate"),
#          type = ifelse(type == "frontier", "Observed", "Projected"),
#          nmx_log = ifelse(round(nmx, 4) == 0, NA, nmx),
#          prefix = getCauseInfo(ghecause, return = "prefix")) %>%
#   select(year, sex, age2, age, prefix, ghecause, causename, type, nmx, nmx_log, concern) %>%
#   arrange(prefix, year, sex, age2, age, type)
#
#
# # * 3.1 All ages ----------------------------------------------------------
#
# # Prepping data for graphing
# ggdata <- temp1
#
# # Graphing parms
# color.info <- groupColorInfo(ggdata, "age2", "age")
# colors <- groupColorFunct(color.info$n, color.info$names, color.and.fill = TRUE)
# linetypes <- c("OLS linear regression" = "solid", "Average mortality rate" = "dashed")
#
# grobs <- list()
# for(i in unique(ggdata$causename)){
#
#   panels <- list()
#   ggdata2 <- ggdata %>% filter(causename == i)
#   id <- ids[which(unique(ggdata$causename) == i)]
#
#   # Standard scale
#   ylims <- ggRange(ggdata2$nmx)
#   panels[["standard"]] <- ggplot(ggdata2) +
#     facet_grid(cols = vars(age2)) +
#     geom_line(data = subset(ggdata2, type == "Projected" & year >= 2020),
#               aes(x = year, y = nmx, color = age, linetype = concern), size = 0.5) +
#     geom_point(data = subset(ggdata2, type == "Observed"),
#                aes(x = year, y = nmx, color = age, fill = age), pch = 21) +
#     labs(title = paste(id, i),
#          subtitle = "Standard scale") +
#     scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
#     scale_y_continuous("Frontier mortality rate (per 100K)", limits = ylims,
#                        breaks = pretty_breaks(5), labels = labelAuto) +
#     scale_color_manual("Age", values = colors$colors) +
#     scale_fill_manual("Age", values = colors$fills) +
#     scale_linetype_manual("Projection method", values = linetypes) +
#     theme(panel.grid.minor.y = element_blank(), legend.position = "bottom") +
#     guides(color = guide_legend(title.position = "top", nrow = 4),
#            linetype = guide_legend(title.position = "top", ncol = 1))
#
#   # Spacer
#   panels[["space"]] <- ""
#
#   # Log scale
#   ylims <- ggRange(ggdata2$nmx_log, log = TRUE)
#   panels[["log"]] <- ggplot(ggdata2) +
#     facet_grid(cols = vars(age2)) +
#     geom_line(data = subset(ggdata2, type == "Projected" & year >= 2020),
#               aes(x = year, y = nmx_log, color = age, linetype = concern), size = 0.5) +
#     geom_point(data = subset(ggdata2, type == "Observed"),
#                aes(x = year, y = nmx_log, color = age, fill = age), pch = 21) +
#     labs(subtitle = "Log scale*") +
#     scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
#     scale_y_continuous("Frontier mortality rate (per 100K)", trans = "log", limits = ylims,
#                        breaks = log.breaks, labels = log.labels) +
#     scale_color_manual("Age", values = colors$colors) +
#     scale_fill_manual("Age", values = colors$fills) +
#     scale_linetype_manual("Projection method", values = linetypes) +
#     theme(axis.title.x = element_blank(),
#           legend.position = "bottom",
#           panel.grid.minor.y = element_blank()) +
#     guides(color = guide_legend(title.position = "top", nrow = 4),
#            linetype = guide_legend(title.position = "top", ncol = 1))
#
#   # Combining panels
#   figure <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.2, 1),
#                       common.legend = TRUE, legend = "bottom") %>%
#     annotate_figure(bottom = text_grob(makeCaption(captions[c("Zero", "Not scaled")]),
#                                        family = "Barlow", size = 11, hjust = 0, x = 0.075))
#
#   grobs[[i]] <- as_grob(figure)
#
# }
#
# saveMultipage(grobs, "frontier_unscaled_all.pdf", width = 11, height = 8.5)
#
#
# # * 3.2 Ages 30 plus ------------------------------------------------------
#
# # Prepping data for graphing
# ggdata <- temp1 %>%
#   filter(age2 %in% levels(temp1$age2)[4:8])
#
# grobs <- list()
# for(i in unique(ggdata$causename)){
#
#   panels <- list()
#   ggdata2 <- ggdata %>% filter(causename == i)
#   id <- ids[which(unique(ggdata$causename) == i)]
#
#   # Standard scale
#   ylims <- ggRange(ggdata2$nmx)
#   panels[["standard"]] <- ggplot(ggdata2) +
#     facet_grid(cols = vars(age2)) +
#     geom_line(data = subset(ggdata2, type == "Projected" & year >= 2020),
#               aes(x = year, y = nmx, color = age, linetype = concern), size = 0.5) +
#     geom_point(data = subset(ggdata2, type == "Observed"),
#                aes(x = year, y = nmx, color = age, fill = age), pch = 21) +
#     labs(title = paste(id, i),
#          subtitle = "Standard scale") +
#     scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
#     scale_y_continuous("Frontier mortality rate (per 100K)", limits = ylims, breaks = pretty_breaks(5),
#                        labels = labelAuto) +
#     scale_color_manual("Age", values = colors$colors) +
#     scale_fill_manual("Age", values = colors$fills) +
#     scale_linetype_manual("Projection method", values = linetypes) +
#     theme(panel.grid.minor.y = element_blank(), legend.position = "bottom") +
#     guides(color = guide_legend(title.position = "top", nrow = 4),
#            linetype = guide_legend(title.position = "top", ncol = 1))
#
#   # Spacer
#   panels[["space"]] <- ""
#
#   # Log scale
#   ylims <- ggRange(ggdata2$nmx_log, log = TRUE)
#   panels[["log"]] <- ggplot(ggdata2) +
#     facet_grid(cols = vars(age2)) +
#     geom_line(data = subset(ggdata2, type == "Projected" & year >= 2020),
#               aes(x = year, y = nmx_log, color = age, linetype = concern), size = 0.5) +
#     geom_point(data = subset(ggdata2, type == "Observed"),
#                aes(x = year, y = nmx_log, color = age, fill = age), pch = 21) +
#     labs(subtitle = "Log scale*") +
#     scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
#     scale_y_continuous("Frontier mortality rate (per 100K)", trans = "log", limits = ylims,
#                        breaks = log.breaks, labels = log.labels) +
#     scale_color_manual("Age", values = colors$colors) +
#     scale_fill_manual("Age", values = colors$fills) +
#     scale_linetype_manual("Projection method", values = linetypes) +
#     theme(axis.title.x = element_blank(),
#           legend.position = "bottom",
#           panel.grid.minor.y = element_blank()) +
#     guides(color = guide_legend(title.position = "top", nrow = 4),
#            linetype = guide_legend(title.position = "top", ncol = 1))
#
#   # Combining panels
#   figure <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.2, 1),
#                       common.legend = TRUE, legend = "bottom") %>%
#     annotate_figure(bottom = text_grob(makeCaption(captions[c("Zero", "Not scaled")]),
#                                        family = "Barlow", size = 11, hjust = 0, x = 0.075))
#
#   grobs[[i]] <- as_grob(figure)
#
# }
#
# saveMultipage(grobs, "frontier_unscaled_30plus.pdf", width = 11, height = 8.5)
#
#
# # 4 END -------------------------------------------------------------------
#
# # Tidying environment and notifying the end of the script
# notifyScript(); tidy()

