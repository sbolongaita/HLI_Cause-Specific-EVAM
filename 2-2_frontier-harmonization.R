
### 2.2 Frontier Harmonization

# This script harmonizes (or scales) the frontier using a level-wise approach,
# such that mortality rates of lower level causes of death sum to mortality
# rates of higher level causes of death. This script relies a function
# (`harmonize`) created in `scr/harmonize.R`.

# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("cause_hierarchy", "frontier_base", "frontier_info/frontier_info_1"), folder = "data/processed")



# 2 Harmonizing the frontier ----------------------------------------------
# Using the `harmonize` function from `scr/harmonize.R`

# Adding cause parents and levels
data <- frontier_base %>%
  left_join(cause_hierarchy %>% select(ghecause, parent_ghecause, parent_causename, level),
            by = "ghecause")


# * 2.1 Level 0 -----------------------------------------------------------
# Reference level (Chang et al. longevity frontier)
harmonized <- data %>%
  filter(level == 0)


# * 2.2 Level 1 -----------------------------------------------------------
# Scaling level 1 frontiers so they sum to the level 0 frontier
lvl1 <- harmonize(1, data, harmonized)$harmonized
harmonized <- bind_rows(harmonized, lvl1)


# * 2.3 Level 2 -----------------------------------------------------------
# Scaling level 2 frontiers so they sum to level 1 frontiers
lvl2 <- harmonize(2, data, harmonized)$harmonized
harmonized <- bind_rows(harmonized, lvl2)


# * 2.4 Level 3 -----------------------------------------------------------
# Scaling level 3 frontiers so they sum to level 2 frontiers
lvl3 <- harmonize(3, data, harmonized)$harmonized
harmonized <- bind_rows(harmonized, lvl3)


# * 2.5 Checking harmonization --------------------------------------------

frontier_harmonized <- harmonized %>%
  select(year, sex, age, ghecause, causename, frontier) %>%
  arrange(year, age, ghecause, sex)

levels <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
concerns <- list()
for(i in levels){

  check <- frontier_harmonized %>%
    left_join(cause_hierarchy %>% select(ghecause, mece = !!as.name(i)), by = "ghecause") %>%
    filter(mece) %>%
    group_by(year, age) %>%
    dplyr::summarize(lower_summed = sum(frontier), .groups = "drop") %>%
    left_join(data %>% filter(level == 0) %>% dplyr::select(year, age, reference = frontier),
              by = c("year", "age")) %>%
    mutate(sf = reference / lower_summed)

  concern <- check %>%
    filter(sf < 0.99 | sf > 1.01) %>%
    arrange(desc(sf))

  if(nrow(concern) > 0){
    warning(paste("Concerning scaling factors:", names(levels[levels == i])))
    concerns[[names(levels[levels == i])]] <- concern
  }
}

# __+ frontier_harmonized -------------------------------------------------
sarahSave("frontier_harmonized", folder = "data/processed")



# 3 Adding to frontier_info dataframe -------------------------------------
frontier_info_2 <- frontier_info_1 %>%
  left_join(frontier_harmonized %>% dplyr::rename(harmonized = frontier),
            by = c("year", "age", "sex", "ghecause", "causename")) %>%
  arrange(year, age, ghecause, sex)

# __+ frontier_info -------------------------------------------------------
sarahSave("frontier_info_2", folder = "data/processed/frontier_info")



# 4 Graphing --------------------------------------------------------------

exit()

ggdata <- frontier_info_2 %>%
  pivot_longer(cols = c("base", "harmonized"), names_to = "stage", values_to = "frontier") %>%
  left_join(cause_hierarchy, by = c("ghecause", "causename")) %>%
  filter(age >= 30, is.na(sex) | sex == 2, year == 2019) %>%
  mutate(stage = capitalize(stage),
         causename = ifelse(causename %in% sex.specific, paste(causename, "(females)"), causename),
         frontier = ifelse(frontier < 0.1, 0, frontier)) %>%
  mutate(display.causename = factor(wrapper(causename, 35), levels = wrapper(causename.levels2, 35))) %>%
  mutate(zero = frontier == 0)

colors <- setNames(colorFunct(4, color.and.fill = TRUE)$colors[c(3,4)], c("Base", "Harmonized"))
fills <- setNames(colorFunct(4, color.and.fill = TRUE)$fills[c(3,4)], c("Base", "Harmonized"))
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
      geom_line(aes(x = age, y = frontier, group = stage, color = stage)) +
      geom_point(aes(x = age, y = frontier, group = stage, color = stage, fill = stage,
                     shape = zero)) +
      scale_x_continuous("Age") +
      scale_y_continuous("Frontier mortality rate (per 100K, log scale)", trans = "log",
                         breaks = log.breaks, labels = log.labels, limits = ylims) +
      scale_color_manual("Frontier estimation stage", values = colors$colors) +
      scale_fill_manual("Frontier estimation stage", values = colors$fills) +
      scale_shape_manual("", values = shapes) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(title.position = "bottom", override.aes = list(pch = 21)),
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

saveGGplot(x = grobs, name = "2-2_frontier-harmonization.pdf",
           folder = "output/figures",
           width = 12, height = 6, multipage = TRUE)

