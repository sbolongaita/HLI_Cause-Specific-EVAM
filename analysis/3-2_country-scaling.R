
### Healthy Longevity Initiative
### 3-2 Country scaling



# 1 ENVIRONMENT -----------------------------------------------------------

applyEnv()

# Loading data
sarahLoad(c("country_info", "country_projected", "population"), folder = "data/processed")
country_projected %<>%
  mutate(dths_rate = ifelse(is.na(dths_rate), projection, dths_rate)) %>%
  dplyr::select(-projection)
population %<>%
  filter(year == 2020) %>%
  dplyr::select(iso3 = iso3.region, total_pop) %>%
  unique()
envelope <- read.csv("data/input/chang_country.csv", as.is = TRUE) %>%
  filter(year >= 2000) %>%
  mutate(ghecause = 0,
         reference = mxn * 100000) %>%
  dplyr::select(iso3, year, sex, age, ghecause, reference)



# 2 SCALING WITH CHANG ET AL.  ENVELOPE -----------------------------------

# Setting the Chang et al.'s longevity frontier as the all-cause frontier
# and adding cause parents and levels
data <- left_join(country_projected, envelope, by = c("iso3", "year", "sex", "age", "ghecause")) %>%
  left_join(cause_hierarchy %>% select(ghecause, parent_ghecause, parent_causename, level),
            by = "ghecause") %>%
  mutate(dths_rate = ifelse(!is.na(reference), reference, dths_rate)) %>%
  select(-reference) %>%
  arrange(year, age, ghecause, sex)


# * 2.1 Level 0 -----------------------------------------------------------
# Reference level (Chang et al. longevity frontier)
scaled <- data %>%
  filter(level == 0)


# * 2.2 Level 1 -----------------------------------------------------------
# Scaling level 1 frontiers so they sum to the level 0 frontier
lvl1 <- scale(1, data, scaled)$scaled
scaled <- bind_rows(scaled, lvl1)


# * 2.3 Level 2 -----------------------------------------------------------
# Scaling level 2 frontiers so they sum to level 1 frontiers
lvl2 <- scale(2, data, scaled)$scaled
scaled <- bind_rows(scaled, lvl2)


# * 2.4 Level 3 -----------------------------------------------------------
# Scaling level 3 frontiers so they sum to level 2 frontiers
lvl3 <- scale(3, data, scaled)$scaled
scaled <- bind_rows(scaled, lvl3)


# * 2.5 Arranging data ----------------------------------------------------
country_scaled <- scaled %>%
  select(iso3, year, sex, age, ghecause, causename, dths_rate)


# * 2.6 Checking scaling --------------------------------------------------

# Checking scaling
levels <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
concerns <- list()
for(i in levels){

  check <- country_scaled %>%
    left_join(cause_hierarchy %>% select(ghecause, mece = !!as.name(i)), by  = "ghecause") %>%
    filter(mece) %>%
    group_by(iso3, year, sex, age) %>%
    dplyr::summarize(lower_summed = sum(dths_rate), .groups = "drop") %>%
    left_join(envelope, by = c("iso3", "year", "sex", "age")) %>%
    mutate(sf = reference / lower_summed)

  concern <- check %>%
    filter(sf < 0.99 | sf > 1.01) %>%
    arrange(desc(sf))

  if(nrow(concern) > 0){
    warning(paste("Concerning scaling factors:", names(levels[levels == i])))
    concerns[[names(levels[levels == i])]] <- concern
  }
}

# __+ frontier_scaled -----------------------------------------------------
sarahSave("country_scaled", folder = "data/processed")



# # 3 GRAPHING --------------------------------------------------------------
#
# # Creating figures objects
# folder <- makeFolder(figures)
#
# # Formatting data for graphing
# temp1 <- country_scaled %>%
#   mutate(age2 = makeMathersAgeGroup(age),
#          age = makeAgeGroup(age),
#          dths_rate_log = ifelse(round(dths_rate, 4) == 0, NA, dths_rate),
#          prefix = getCauseInfo(ghecause, return = "prefix")) %>%
#   select(iso3, year, sex, age2, age, prefix, ghecause, causename, dths_rate, dths_rate_log) %>%
#   arrange(prefix, iso3, year, age2, age, sex)
#
# # Graphing parms
# color.info <- groupColorInfo(temp1, "age2", "age")
# colors <- groupColorFunct(color.info$n, color.info$names, color.and.fill = TRUE)
#
# # Selecting China, India, and the largest country by population of the other
# # regions
# iso3s <- temp1 %>% select(iso3) %>% unique() %>%
#   left_join(country_info, by = "iso3") %>%
#   left_join(population, by = "iso3") %>%
#   group_by(region) %>%
#   arrange(desc(total_pop)) %>%
#   slice_head(n = 1) %>%
#   pull(iso3)
#
# # Prepping data for graphing
# ggdata <- temp1 %>%
#   filter(iso3 %in% iso3s, age2 %in% levels(temp1$age2)[4:8])
#
# color.info <- unique(ggdata$age)
# colors$colors <- colors$colors[color.info]
# colors$fills <- colors$fills[color.info]
#
# for(j in unique(ggdata$iso3)){
#
#   grobs <- list()
#
#   for(i in unique(ggdata$causename)){
#
#     ggdata2 <- ggdata %>% filter(iso3 == j, causename == i)
#     id <- ids[which(unique(ggdata$causename) == i)]
#     ylims <- ggRange(ggdata2$dths_rate); ylims.log <- ggRange(ggdata2$dths_rate_log, log = TRUE)
#
#     Panels <- list()
#     for(k in unique(ggdata2$sex)){
#
#       panels <- list()
#       ggdata3 <- ggdata2 %>% filter(sex == k)
#       title <- ifelse(k == 2, paste(id, i), " ")
#       sex <- ifelse(k == 1, "Males", "Females")
#
#       # Standard scale
#       panels[["standard"]] <- ggplot(ggdata3) +
#         facet_grid(cols = vars(age2)) +
#         geom_line(data = subset(ggdata3, year >= 2020),
#                   aes(x = year, y = dths_rate, color = age), size = 0.5) +
#         geom_point(data = subset(ggdata3, year < 2020),
#                    aes(x = year, y = dths_rate, color = age, fill = age)) +
#         labs(title = title,
#              subtitle = paste0(sex, "   |   Standard scale")) +
#         scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
#         scale_y_continuous("Mortality rate (per 100K)", limits = ylims, breaks = pretty_breaks(5),
#                            labels = labelAuto) +
#         scale_color_manual("Age", values = colors$colors) +
#         scale_fill_manual("Age", values = colors$fills) +
#         theme(axis.title.x = element_blank(), legend.position = "bottom",
#               panel.grid.minor.y = element_blank()) +
#         guides(color = guide_legend(title.position = "top", nrow = 4))
#
#       # Spacer
#       panels[["space"]] <- ""
#
#       # Log scale
#       panels[["log"]] <- ggplot(ggdata3) +
#         facet_grid(cols = vars(age2)) +
#         geom_line(data = subset(ggdata3, year >= 2020),
#                   aes(x = year, y = dths_rate, color = age), size = 0.5) +
#         geom_point(data = subset(ggdata3, year < 2020),
#                    aes(x = year, y = dths_rate_log, color = age, fill = age), pch = 21) +
#         labs(subtitle = paste0(sex, "   |   Log scale*")) +
#         scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
#         scale_y_continuous("Mortality rate (per 100K)", trans = "log", limits = ylims.log,
#                            breaks = log.breaks, labels = log.labels) +
#         scale_color_manual("Age", values = colors$colors) +
#         scale_fill_manual("Age", values = colors$fills) +
#         theme(axis.title.x = element_blank(), legend.position = "bottom",
#               panel.grid.minor.y = element_blank()) +
#         guides(color = guide_legend(title.position = "top", nrow = 4))
#
#       Panels[[sex]] <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.15, 1),
#                           common.legend = TRUE, legend = "none")
#
#     }
#
#     figure <- ggarrange(plotlist = Panels[c(2, 1)], ncol = 2, align = "hv",
#                         common.legend = TRUE, legend = "bottom", legend.grob = get_legend(panels[[1]])) %>%
#       annotate_figure(bottom = text_grob(makeCaption(captions[c("Zero", "Scaled")]),
#                                          family = "Barlow", size = 11, hjust = 0, x = 0.075))
#
#     grobs[[i]] <- as_grob(figure)
#
#   }
#
#   saveMultipage(grobs, paste0(j, "_scaled_30plus.pdf"), width = 11, height = 8.5)
#
# }
#
#
# # 3 ENDING ----------------------------------------------------------------
#
# # Tidying environment and notifying the end of the script
# notifyScript(); tidy()
