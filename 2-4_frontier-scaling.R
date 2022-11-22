
### 2.4 Frontier scaling



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("cause_hierarchy", "frontier_projected", "frontier_info/frontier_info_3"),
          folder = "data/processed")
envelope <- read.csv("data/input/chang_frontier.csv", as.is = TRUE) %>%
  mutate(reference = mxn * 100000) %>%
  select(year, age, reference)



# 2 Scaling with Chang et al. envelope ------------------------------------

# Setting the Chang et al.'s longevity frontier as the all-cause frontier
# and adding cause parents and levels
temp1 <- frontier_projected %>%
  mutate(frontier = ifelse(is.na(frontier), projection, frontier)) %>%
  left_join(envelope %>% mutate(ghecause = 0, causename = "All causes"),
            by = c("year", "age", "ghecause", "causename"))

chang_scaling_factors <- temp1 %>%
  filter(!is.na(reference)) %>%
  mutate(sf = reference / frontier)

# __+ chang_scaling_factors -------------------------------------------
sarahSave("chang_scaling_factors", folder = "data/processed")

data <- temp1 %>%
  left_join(cause_hierarchy %>% select(ghecause, parent_ghecause, parent_causename, level),
            by = "ghecause") %>%
  mutate(frontier = ifelse(!is.na(reference), reference, frontier)) %>%
  select(-c(projection, reference)) %>%
  arrange(year, age, ghecause, sex)


# * 2.1 L0 ----------------------------------------------------------------
# Reference level (Chang et al. longevity frontier)
scaled <- data %>%
  filter(level == 0)


# * 2.2 L1 ----------------------------------------------------------------
# Scaling level 1 frontiers so they sum to the level 0 frontier
lvl1 <- harmonize(1, data, scaled)$harmonized
scaled <- bind_rows(scaled, lvl1)


# * 2.3 L2 ----------------------------------------------------------------
# Scaling level 2 frontiers so they sum to level 1 frontiers
lvl2 <- harmonize(2, data, scaled)$harmonized
scaled <- bind_rows(scaled, lvl2)


# * 2.4 L3 ----------------------------------------------------------------
# Scaling level 3 frontiers so they sum to level 2 frontiers
lvl3 <- harmonize(3, data, scaled)$harmonized
scaled <- bind_rows(scaled, lvl3)


# * 2.5 Arranging data ----------------------------------------------------
frontier_scaled <- scaled %>%
  select(year, age, sex, ghecause, causename, frontier) %>%
  arrange(year, age, ghecause, sex)



# * 2.6 Checking scaling --------------------------------------------------
# Checking harmonization
levels <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
concerns <- list()
for(i in levels){

  check <- frontier_scaled %>%
    left_join(cause_hierarchy %>% select(ghecause, mece = !!as.name(i)), by  = "ghecause") %>%
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
sarahSave("frontier_scaled", folder = "data/processed")


# * 2.7 Adding to frontier_info dataframe ----------------------------------
frontier_info_4 <- frontier_info_3 %>%
  full_join(frontier_scaled %>% dplyr::rename(scaled = frontier),
            by = c("year", "age", "sex", "ghecause", "causename")) %>%
  arrange(age, ghecause, year, sex)

# __+ frontier_info -------------------------------------------------------
sarahSave("frontier_info_4", folder = "data/processed/frontier_info")



# 3 GRAPHING --------------------------------------------------------------

# Formatting data for graphing
ggdata <- left_join(frontier_projected %>%
                      dplyr::rename(harmonized = frontier, projected = projection) %>%
                      mutate(age2 = makeMathersAgeGroup(age),
                             age3 = makeDisplayAgeGroup(age)),
                    frontier_scaled %>% dplyr::rename(scaled = frontier),
                    by = c("year", "age", "sex", "ghecause", "causename")) %>%
  pivot_longer(cols = c("harmonized", "projected", "scaled"), names_to = "stage", values_to = "frontier") %>%
  left_join(cause_hierarchy %>% select(ghecause, causename, parent_causename), by = c("ghecause", "causename")) %>%
  filter(!is.na(frontier), age >= 30, is.na(sex) | sex == 2, year == 2019) %>%
  mutate(stage = capitalize(stage),
         display.causename = factor(wrapper(causename, 30), levels = wrapper(causename.levels, 30)),
         frontier = ifelse(frontier < 0.1, 0, frontier)) %>%
  mutate(zero = frontier == 0)

colors <- setNames(colorFunct(6, color.and.fill = TRUE)$colors[c(5, 5, 6)], c("Harmonized", "Projected", "Scaled"))
fills <- setNames(colorFunct(6, color.and.fill = TRUE)$fills[c(5, 5, 6)], c("Harmonized", "Projected", "Scaled"))
colors <- list(colors = colors, fills = fills)

shapes <- c(21, NA)

grobs <- list()
for(parent in unique(ggdata$parent_causename)[!is.na(unique(ggdata$parent_causename))]){

  ggdata2 <- ggdata %>% filter(causename == parent | parent_causename == parent)
  ggdata.parent <- ggdata %>% filter(causename == parent)
  ggdata.children <- ggdata %>% filter(parent_causename == parent)
  ggdata.list <- list("parent" = ggdata.parent, "children" = ggdata.children)

  n.children <- length(unique(ggdata.children$causename))
  children.row <- round_any(n.children, 3, ceiling) / 3
  children.margin <- case_when(n.children == 2 ~ c(10, 170, 220, 10),
                               children.row == 1 ~ c(10, 10, 220, 10),
                               children.row == 2 ~ c(10, 10, 100, 10),
                               children.row == 3 ~ c(10, 10, 31, 10))

  id <- ids[which(unique(ggdata$parent_causename)[!is.na(unique(ggdata$parent_causename))] == parent)]
  ylims <- c(0.1, ggRange(ggdata2$frontier)[2] * 1.1)

  figures <- list()
  for(i in names(ggdata.list)){

    figures[[i]] <- ggplot(ggdata.list[[i]]) +
      geom_line(data = subset(ggdata.list[[i]], stage %in% c("Projected", "Scaled")),
                aes(x = age, y = frontier, group = stage, color = stage)) +
      geom_point(data = subset(ggdata.list[[i]], stage == "Harmonized"),
                 aes(x = age, y = frontier, group = stage, color = stage, fill = stage,
                     shape = zero)) +
      scale_x_continuous("Age") +
      scale_y_continuous("Frontier mortality rate (per 100K, log scale)", trans = "log",
                         breaks = log.breaks, labels = log.labels, limits = ylims) +
      scale_color_manual("Frontier estimation stage", values = colors$colors) +
      scale_fill_manual("Frontier estimation stage", values = colors$fills) +
      scale_shape_manual("", values = shapes) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(title.position = "bottom", override.aes = list(pch = c(21, NA, NA), linetype = c(NA, 1, 1))),
             fill = guide_legend(title.position = "bottom"),
             shape = "none")

    if(i == "parent"){
      figures[[i]] <- figures[[i]] +
        labs(title = paste(id, parent), subtitle = "2019")
    }else{
      figures[[i]] <- figures[[i]] +
        labs(title = "", subtitle = "Lower level causes") +
        facet_wrap(~ display.causename, ncol = 3, nrow = children.row) +
        theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
              axis.text = element_text(size = 9),
              legend.position = "none",
              plot.margin = margin(children.margin),
              strip.text = element_text(size = 9, hjust = 0.5,
                                        vjust = 0.5, margin = margin(2, 2, 2, 2)))
    }

  }

  grobs[[parent]] <- ggarrange(plotlist = figures, ncol = 2, nrow = 1, widths = c(1, 1.5),
                               common.legend = TRUE, legend = "bottom")

}

saveGGplot(x = grobs, name = "2-4_frontier-scaling.pdf", folder = "output/figures",
           width = 12, height = 6, multipage = TRUE)
