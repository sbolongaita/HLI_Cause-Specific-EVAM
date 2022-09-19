
### Healthy Longevity Initiative
### 230 - FRONTIER SCALING

# This script takes the harmonized and projected frontier mortality rates
# and scales them with UN Pop and Chang et al.'s longevity frontiers.

# Returns:
# • output_data/frontier_scaled.Rda (R dataset)


# 1 ENVIRONMENT -----------------------------------------------------------

# Clearing and loading environment
tidy(); sourceEnv()
scriptName <- "230-frontier_scaling"

# Loading data and the harmonize function from the '210-frontier_analysis.R'
sarahLoad(c("frontier_projected", "harmonize"))
envelope <- read.csv("02-input_data/Chang_frontier-life-tables.csv", as.is = TRUE) %>%
  mutate(year = floor(year), reference = mxn * 100000) %>%
  filter(year >= 2000) %>%
  select(year, age, reference)



# 2 SCALING WITH CHANG ET AL. ENVELOPE ------------------------------------

# Setting the Chang et al.'s longevity frontier as the all-cause frontier
# and adding cause parents and levels
temp1 <- frontier_projected %>%
  mutate(frontier = ifelse(is.na(frontier), projection, frontier)) %>%
  left_join(envelope %>% mutate(ghecause = 0, causename = "All causes"),
            by = c("year", "age", "ghecause", "causename"))

longevity_scaling_factors <- temp1 %>%
  filter(!is.na(reference)) %>%
  mutate(sf = reference / frontier)

# __+ longevity_scaling_factors -------------------------------------------
sarahSave("longevity_scaling_factors")

data <- temp1 %>%
  mutate(frontier = ifelse(!is.na(reference), reference, frontier),
         parent_ghecause = getCauseInfo(ghecause, return = "parent_ghecause"),
         parent_causename = getCauseInfo(ghecause, return = "parent_causename"),
         level = getCauseInfo(ghecause, return = "level")) %>%
  select(-c(projection, reference)) %>%
  arrange(year, age, ghecause, sex)


# * 2.1 Level 0 -----------------------------------------------------------
# Reference level (Chang et al. longevity frontier)
scaled <- data %>%
  filter(level == 0)


# * 2.2 Level 1 -----------------------------------------------------------
# Scaling level 1 frontiers so they sum to the level 0 frontier
lvl1 <- harmonize(1, data, scaled)$harmonized
scaled <- bind_rows(scaled, lvl1)


# * 2.3 Level 2 -----------------------------------------------------------
# Scaling level 2 frontiers so they sum to level 1 frontiers
lvl2 <- harmonize(2, data, scaled)$harmonized
scaled <- bind_rows(scaled, lvl2)


# * 2.4 Level 3 -----------------------------------------------------------
# Scaling level 3 frontiers so they sum to level 2 frontiers
lvl3 <- harmonize(3, data, scaled)$harmonized
scaled <- bind_rows(scaled, lvl3)


# * 2.5 Arranging data ----------------------------------------------------
Order <- order[order %in% names(scaled)]
Sort <- sort[sort %in% names(scaled)]
frontier_scaled <- scaled %>%
  dplyr::select(all_of(Order), frontier) %>%
  dplyr::arrange(!!!rlang::parse_exprs(Sort))


# * 2.6 Checking scaling --------------------------------------------------
# Checking harmonization
levels <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
concerns <- list()
for(i in levels){

  check <- frontier_scaled %>%
    mutate(mece = getCauseInfo(ghecause, return = i)) %>%
    filter(mece) %>%
    group_by(year, age) %>%
    dplyr::summarize(lower_summed = sum(frontier), .groups = "drop") %>%
    left_join(envelope, by = c("year", "age")) %>%
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
sarahSave("frontier_scaled", type = "both")



# 3 GRAPHING --------------------------------------------------------------

# Creating figures objects
folder <- makeFolder(figures)

# Formatting data for graphing
temp1 <- frontier_scaled %>%
  filter(is.na(sex) | sex == 2) %>%
  mutate(age2 = makeMathersAgeGroup(age),
         age = makeAgeGroup(age),
         causename = ifelse(is.na(sex), causename, paste(causename, "(Females)")),
         frontier_log = ifelse(round(frontier, 4) == 0, NA, frontier),
         prefix = getCauseInfo(ghecause, return = "prefix")) %>%
  select(year, sex, age2, age, prefix, ghecause, causename, frontier, frontier_log) %>%
  arrange(prefix, year, sex, age2, age)


# * 3.1 All ages ----------------------------------------------------------

# Prepping data for graphing
ggdata <- temp1

# Graphing parms
color.info <- groupColorInfo(ggdata, "age2", "age")
colors <- groupColorFunct(color.info$n, color.info$names, color.and.fill = TRUE)

grobs <- list()
for(i in unique(ggdata$causename)){

  ggdata2 <- ggdata %>% filter(causename == i)

  panels <- list()
  id <- ids[which(unique(ggdata$causename) == i)]

  # Standard scale
  ylims <- ggRange(ggdata2$frontier)
  panels[["standard"]] <- ggplot(ggdata2) +
    facet_grid(cols = vars(age2)) +
    geom_line(data = subset(ggdata2, year >= 2020),
              aes(x = year, y = frontier, color = age), size = 0.5) +
    geom_point(data = subset(ggdata2, year < 2020),
               aes(x = year, y = frontier, color = age, fill = age), pch = 21) +
    labs(title = paste(id, i),
         subtitle = "Standard scale") +
    scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
    scale_y_continuous("Frontier mortality rate (per 100K)", limits = ylims, breaks = pretty_breaks(5),
                       labels = labelAuto) +
    scale_color_manual("Age", values = colors$colors) +
    scale_fill_manual("Age", values = colors$fills) +
    theme(panel.grid.minor.y = element_blank(), legend.position = "bottom") +
    guides(color = guide_legend(title.position = "top", nrow = 4),
           linetype = guide_legend(title.position = "top", ncol = 1))

  # Spacer
  panels[["space"]] <- ""

  # Log scale
  ylims <- ggRange(ggdata2$frontier_log, log = TRUE)
  panels[["log"]] <- ggplot(ggdata2) +
    facet_grid(cols = vars(age2)) +
    geom_line(data = subset(ggdata2, year >= 2020),
              aes(x = year, y = frontier_log, color = age), size = 0.5) +
    geom_point(data = subset(ggdata2, year < 2020),
               aes(x = year, y = frontier_log, color = age, fill = age), pch = 21) +
    labs(subtitle = "Log scale*") +
    scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
    scale_y_continuous("Frontier mortality rate (per 100K)", trans = "log", limits = ylims,
                       breaks = log.breaks, labels = log.labels) +
    scale_color_manual("Age", values = colors$colors) +
    scale_fill_manual("Age", values = colors$fills) +
    theme(panel.grid.minor.y = element_blank(), legend.position = "bottom") +
    guides(color = guide_legend(title.position = "top", nrow = 4),
           linetype = guide_legend(title.position = "top", ncol = 1))

  # Combining panels
  figure <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.2, 1),
                      common.legend = TRUE, legend = "bottom") %>%
    annotate_figure(bottom = text_grob(makeCaption(captions[c("Zero", "Scaled")]),
                                       family = "Barlow", size = 11, hjust = 0, x = 0.075))

  grobs[[i]] <- as_grob(figure)

}

saveMultipage(grobs, "frontier_scaled_all.pdf", width = 11, height = 8.5)


# * 3.2 Ages 30 plus ------------------------------------------------------

# Prepping data for graphing
ggdata <- temp1 %>%
  filter(age2 %in% levels(temp1$age2)[4:8])
color.info <- unique(ggdata$age)
colors$colors <- colors$colors[color.info]
colors$fills <- colors$fills[color.info]

grobs <- list()
for(i in unique(ggdata$causename)){

  ggdata2 <- ggdata %>% filter(causename == i)

  panels <- list()
  id <- ids[which(unique(ggdata$causename) == i)]

  # Standard scale
  ylims <- ggRange(ggdata2$frontier)
  panels[["standard"]] <- ggplot(ggdata2) +
    facet_grid(cols = vars(age2)) +
    geom_line(data = subset(ggdata2, year >= 2020),
              aes(x = year, y = frontier, color = age), size = 0.5) +
    geom_point(data = subset(ggdata2, year < 2020),
               aes(x = year, y = frontier, color = age, fill = age), pch = 21) +
    labs(title = paste(id, i),
         subtitle = "Standard scale") +
    scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
    scale_y_continuous("Frontier mortality rate (per 100K)", limits = ylims, breaks = pretty_breaks(5),
                       labels = labelAuto) +
    scale_color_manual("Age", values = colors$colors) +
    scale_fill_manual("Age", values = colors$fills) +
    theme(panel.grid.minor.y = element_blank(), legend.position = "bottom") +
    guides(color = guide_legend(title.position = "top", nrow = 4),
           linetype = guide_legend(title.position = "top", ncol = 1))

  # Spacer
  panels[["space"]] <- ""

  # Log scale
  ylims <- ggRange(ggdata2$frontier_log, log = TRUE)
  panels[["log"]] <- ggplot(ggdata2) +
    facet_grid(cols = vars(age2)) +
    geom_line(data = subset(ggdata2, year >= 2020),
              aes(x = year, y = frontier_log, color = age), size = 0.5) +
    geom_point(data = subset(ggdata2, year < 2020),
               aes(x = year, y = frontier_log, color = age, fill = age), pch = 21) +
    labs(subtitle = "Log scale*") +
    scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
    scale_y_continuous("Frontier mortality rate (per 100K)", trans = "log", limits = ylims,
                       breaks = log.breaks, labels = log.labels) +
    scale_color_manual("Age", values = colors$colors) +
    scale_fill_manual("Age", values = colors$fills) +
    theme(panel.grid.minor.y = element_blank(), legend.position = "bottom") +
    guides(color = guide_legend(title.position = "top", nrow = 4),
           linetype = guide_legend(title.position = "top", ncol = 1))

  # Combining panels
  figure <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.2, 1),
                      common.legend = TRUE, legend = "bottom") %>%
    annotate_figure(bottom = text_grob(makeCaption(captions[c("Zero", "Scaled")]),
                                       family = "Barlow", size = 11, hjust = 0, x = 0.075))

  grobs[[i]] <- as_grob(figure)

}

saveMultipage(grobs, "frontier_scaled_30plus.pdf", width = 11, height = 8.5)



# 3 END -------------------------------------------------------------------

# Tidying environment and notifying the end of the script
notifyScript(); tidy()
