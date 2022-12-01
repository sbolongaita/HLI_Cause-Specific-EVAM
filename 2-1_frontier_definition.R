
### 2.1 Frontier Definition

# This script takes the recoded GHE data for frontier-eligible countries,
# calculates age-cause-specific mortality rates, and the extracts the frontier
# using two definitions: the minimum and the 10th percentile. It selects the
# 10th percentile as the definition for use in the subsequent analysis and
# creates a `data/processed/frontier_info` folder, in which the 'frontier' is
# tracked throughout each subsequent step of its calculation (i.e., scripts
# with prefix 2-).


# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("cause_hierarchy", "country_info", "ghe_recoded", "population"),
          folder = "data/processed")



# 2 Extracting the frontier -----------------------------------------------

# Calculating age-cause-sex-year-specific mortality rates for frontier-eligible
# countries
ghe_recoded %<>%
  left_join(population %>% select(iso3, year, sex, age, pop),
            by = c("iso3", "year", "sex", "age")) %>%
  mutate(dths_rate = dths / pop * 100000)

# Extracting the minimum
frontier_min <- ghe_recoded %>%
  mutate(sex = ifelse(causename %in% sex.specific, sex, NA)) %>%
  group_by(year, age, ghecause, causename, sex) %>%
  dplyr::summarize(frontier = min(dths_rate), .groups = "drop") %>%
  dplyr::select(year, sex, age, ghecause, causename, everything()) %>%
  arrange(year, age, ghecause, sex)

# Extracting the 10th percentile
frontier_10p <- ghe_recoded %>%
  mutate(sex = ifelse(causename %in% sex.specific, sex, NA)) %>%
  group_by(year, age, ghecause, causename, sex) %>%
  dplyr::summarize(frontier = unname(quantile(dths_rate, probs = 0.1, type = 3)),
                   .groups = "drop") %>%
  dplyr::select(year, sex, age, ghecause, causename, everything()) %>%
  arrange(year, age, ghecause, sex)

# Selecting the 10th percentile definition as the frontier
frontier_base <- frontier_10p

# __+ frontier_base -------------------------------------------------------
sarahSave("frontier_base", folder = "data/processed")


# 3 Creating a frontier info dataframe ------------------------------------
# Documenting information about the frontier

# Country-sex groups at the minimum age-cause-specific frontier
temp1 <- ghe_recoded %>%
  dplyr::mutate(sex_match = ifelse(causename %in% sex.specific, sex, NA)) %>%
  left_join(frontier_min %>% rename(sex_match = sex),
            by = c("year", "sex_match", "age", "ghecause", "causename")) %>%
  filter(dths_rate == frontier) %>%
  mutate(iso3.sex = paste(iso3, sex)) %>%
  group_by(year, age, sex_match, ghecause, causename) %>%
  summarize(min = min(dths_rate),
            n = n(),
            min_iso3.sex = paste(sort(unique(iso3.sex)), collapse = ", "),
            .groups = "drop") %>%
  mutate(min_iso3.sex = ifelse(n > 5, "5+ country-sex groups", min_iso3.sex)) %>%
  select(-n)

# Country-sex groups at the 10th percentile age-cause-specific frontier
temp2 <- ghe_recoded %>%
  dplyr::mutate(sex_match = ifelse(causename %in% sex.specific, sex, NA)) %>%
  left_join(frontier_10p %>% rename(sex_match = sex),
            by = c("year", "sex_match", "age", "ghecause", "causename")) %>%
  filter(dths_rate == frontier) %>%
  mutate(iso3.sex = paste(iso3, sex)) %>%
  group_by(year, age, sex_match, ghecause, causename) %>%
  summarize(p10 = min(dths_rate),
            n = n(),
            p10_iso3.sex = paste(sort(unique(iso3.sex)), collapse = ", "),
            .groups = "drop") %>%
  mutate(p10_iso3.sex = ifelse(n > 5, "5+ country-sex groups", p10_iso3.sex)) %>%
  select(-n)

# Frontier_info dataframe
frontier_info_1 <- full_join(temp1, temp2, by = c("year", "age", "sex_match", "ghecause", "causename")) %>%
  dplyr::rename(sex = sex_match) %>%
  mutate(base = p10) %>%
  arrange(year, age, ghecause, sex)

# __+ frontier_info --------------------------------------------------------
if(!dir.exists("data/processed/frontier_info")){
  dir.create("data/processed/frontier_info")
}
sarahSave("frontier_info_1", folder = "data/processed/frontier_info")


# 4 Graphing --------------------------------------------------------------

exit()

# * 4.1 A - Distribution --------------------------------------------------

ggdata <- ghe_recoded %>%
  filter(is.na(sex) | sex == 2, age >= 30) %>%
  mutate(age2 = makeDisplayAgeGroup(age),
         sex = ifelse(causename %in% sex.specific, sex, NA),
         causename = ifelse(causename %in% sex.specific, paste(causename, "(females)"), causename)) %>%
  mutate(age = makeAgeGroup(age),
         causename = factor(causename, levels = causename.levels2))

color.info <- groupColorInfo(ggdata, "age2", "age")
colors <- groupColor(n = color.info$n, names = color.info$names, color.and.fill = TRUE)

grobs <- list()
for(i in unique(ggdata$causename)){

  ggdata1 <- ggdata %>% filter(causename == i, year == 2019)
  ggdata2 <- frontier_min %>% mutate(causename = ifelse(causename %in% sex.specific, paste(causename, "(females)"), causename)) %>%
    filter(causename == i, year == 2019, age >= 30, is.na(sex) | sex == 2) %>% mutate(age = makeAgeGroup(age))
  ggdata3 <- frontier_10p %>% mutate(causename = ifelse(causename %in% sex.specific, paste(causename, "(females)"), causename)) %>%
    filter(causename == i, year == 2019, age >= 30, is.na(sex) | sex == 2) %>% mutate(age = makeAgeGroup(age))

  id <- ids[which(unique(ggdata$causename) == i)]

  base <- ggplot() +
    facet_wrap(~ age, scales = "free") +
    geom_density(data = ggdata1, aes(x = dths_rate, fill = age, color = age), trim = TRUE)

  label.y <- ggplot_build(base)$data[[1]] %>% group_by(group) %>%
    summarize(yend = max(ymax)) %>%
    mutate(u = yend / 5, age = unique(ggdata$age)) %>%
    ungroup() %>% select(-group)
  ggdata2 <- left_join(ggdata2, label.y, by = "age") %>% mutate(yend = yend + (2 * u))
  ggdata3 <- left_join(ggdata3, label.y, by = "age") %>% mutate(yend = yend + u)

  grobs[[i]] <- ggplot() +
    facet_wrap(~ age, scales = "free") +
    geom_density(data = ggdata1, aes(x = dths_rate, fill = age, color = age), trim = TRUE) +
    geom_segment(data = ggdata2, aes(x = frontier, xend = frontier, y = 0, yend = yend, color = age)) +
    geom_segment(data = ggdata3, aes(x = frontier, xend = frontier, y = 0, yend = yend, color = age)) +
    geom_label(data = ggdata2, aes(x = (frontier * 1.15), y = yend,
                                   label = paste("Minimum:", format(round(frontier), big.mark = ",", trim = TRUE))),
               hjust = 0, vjust = 0.5, label.size = 0, label.padding = unit(0.1, "lines"), family = "Barlow") +
    geom_label(data = ggdata3, aes(x = (frontier * 1.15), y = yend,
                                   label = paste("10th percentile:", format(round(frontier), big.mark = ",", trim = TRUE))),
               hjust = 0, vjust = 0.5, label.size = 0, label.padding = unit(0.1, "lines"), family = "Barlow") +
    labs(title = paste(id, i), subtitle = "2019") +
    scale_x_continuous("Mortality rate (per 100K)", labels = label_number(scale_cut = cut_short_scale())) +
    scale_y_continuous("Density", expand = expansion(mult = c(0, 0.05))) +
    scale_color_manual("Age group", values = colors$colors) +
    scale_fill_manual("Age group", values = alpha(colors$fills, 0.8)) +
    theme(axis.line.y = element_blank(), axis.text.y = element_blank(), legend.position = "none")

}

saveGGplot(x = grobs, name = "2-1_frontier-definition_distribution.pdf",
           folder = "output/figures", width = 12, height = 10, multipage = TRUE)


# * 4.2 B - Lines ---------------------------------------------------------

ggdata <- frontier_info_1 %>%
  left_join(cause_hierarchy %>% select(ghecause, causename, parent_causename),
            by = c("ghecause", "causename")) %>%
  pivot_longer(cols = c(min, p10), names_to = "definition", values_to = "frontier") %>%
  filter(age >= 30, is.na(sex) | sex == 2, year == 2019) %>%
  mutate(causename = ifelse(causename %in% sex.specific, paste(causename, "(females)"), causename),
         parent_causename = ifelse(parent_causename %in% sex.specific, paste(parent_causename, "(females)"), parent_causename)) %>%
  mutate(display.causename = factor(wrapper(causename, 35),
                                    levels = wrapper(causename.levels2, 35)),
         definition = ifelse(definition == "min", "Minimum", "10th percentile"),
         frontier = ifelse(frontier < 0.1, 0, frontier)) %>%
  mutate(zero = frontier == 0)

colors <- colorFunct(2, names = c("10th percentile", "Minimum"), color.and.fill = TRUE)
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

  ylims <- c(0.1, ggRange(ggdata2$frontier)[2] * 1.1)

  figures <- list()
  for(i in names(ggdata.list)){

    base <- ggplot(ggdata.list[[i]]) +
      geom_line(aes(x = age, y = frontier, group = definition, color = definition)) +
      geom_point(aes(x = age, y = frontier, group = definition, color = definition,
                     fill = definition, shape = zero)) +
      scale_x_continuous("Age") +
      scale_y_continuous("Frontier mortality rate (per 100K, log scale)", trans = "log",
                         breaks = log.breaks, labels = log.labels, limits = ylims) +
      scale_color_manual("Frontier definition", values = colors$colors,
                         breaks = c("Minimum", "10th percentile")) +
      scale_fill_manual("Frontier definition", values = colors$fills,
                         breaks = c("Minimum", "10th percentile")) +
      scale_shape_manual("", values = shapes) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(title.position = "bottom", override.aes = list(pch = 21)),
             fill = guide_legend(title.position = "bottom"),
             shape = "none")

    id <- ids[which(unique(ggdata$parent_causename)[!is.na(unique(ggdata$parent_causename))] == parent)]

    if(i == "parent"){
      figures[[i]] <- base +
        labs(title = paste(id, parent), subtitle = "2019")
    }else{
      figures[[i]] <- base +
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

saveGGplot(x = grobs, name = "2-1_frontier-definition_lines.pdf",
           folder = "output/figures",
           width = 12, height = 6, multipage = TRUE)

