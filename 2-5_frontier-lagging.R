
### 2.5 Frontier lagging



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("frontier_info/frontier_info_4", "frontier_scaled"), folder = "data/processed")



# 2 Lagging the frontier --------------------------------------------------

frontier_lagged <- frontier_scaled %>%
  mutate(year = year - 5)

# __+ frontier_lagged -----------------------------------------------------
sarahSave("frontier_lagged", folder = "data/processed")


# * 2.1 Adding to frontier_info dataframe ----------------------------------
frontier_info_5 <- frontier_info_4 %>%
  full_join(frontier_lagged %>% dplyr::rename(lagged = frontier),
            by = c("year", "age", "sex", "ghecause", "causename")) %>%
  arrange(age, ghecause, year, sex)

# Creating final frontier_info dataframe
frontier_info <- frontier_info_5

# __+ frontier_info -------------------------------------------------------
sarahSave(c("frontier_info_5", "frontier_info"), folder = "data/processed/frontier_info")
sarahSave("frontier_info", folder = "data/processed")



# 3 GRAPHING --------------------------------------------------------------

# * 3.1 A Lagging ---------------------------------------------------------

# Formatting data for graphing
ggdata <- left_join(frontier_scaled %>%
                      dplyr::rename(scaled = frontier) %>%
                      mutate(age2 = makeMathersAgeGroup(age),
                             age3 = makeDisplayAgeGroup(age)),
                    frontier_lagged %>% dplyr::rename(lagged = frontier),
                    by = c("year", "age", "sex", "ghecause", "causename")) %>%
  pivot_longer(cols = c(scaled, lagged), names_to = "type", values_to = "frontier") %>%
  filter(year >= 2010, year <= 2040, age >= 30, is.na(sex) | sex == 2, !is.na(frontier)) %>%
  left_join(cause_hierarchy %>% select(ghecause, prefix), by = "ghecause") %>%
  mutate(age = makeAgeGroup(age),
         causename = ifelse(is.na(sex), causename, paste(causename, "(Females)")),
         type = ifelse(type == "scaled", "Scaled", "Lagged"),
         frontier = ifelse(frontier < 0.1, 0, frontier)) %>%
  mutate(zero = frontier == 0) %>%
  select(year, sex, age3, age2, age, prefix, ghecause, causename, type, frontier, zero) %>%
  arrange(prefix, year, sex, age3, age2, age, type)

color.info <- groupColorInfo(ggdata, "age3", "age")
colors <- groupColor(color.info$n, color.info$names, color.and.fill = TRUE)
linetypes <- c("Scaled" = "dashed", "Lagged" = "solid")
shapes <- c(21, NA)

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
    geom_line(data = subset(ggdata2, type == "Scaled" & year >= 2010),
              aes(x = year, y = frontier, color = age, linetype = type), size = 0.5) +
    geom_line(data = subset(ggdata2, type == "Lagged" & year >= 2010),
              aes(x = year, y = frontier, color = age, linetype = type), size = 0.5) +
    scale_x_continuous("", breaks = seq(2010, 2040, 10),
                       labels = c("2010", "'20", "'30", "'40")) +
    scale_color_manual("Age", values = colors$colors) +
    scale_fill_manual("Age", values = colors$fills) +
    scale_linetype_manual("Frontier estimation stage", values = linetypes) +
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

saveGGplot(grobs, "2-5_frontier-lagging_A.pdf", folder = "output/figures",
           width = 11, height = 8.5, multipage = TRUE)



# * 3.2 B Full process ----------------------------------------------------

ggdata <- frontier_info %>%
  filter(age >= 30, is.na(sex) | sex == 2) %>%
  mutate(age = makeAgeGroup(age),
         causename = ifelse(causename %in% sex.specific, paste(causename, "(females)"), causename)) %>%
  mutate(causename = factor(causename, levels = causename.levels2)) %>%
  pivot_longer(cols = c(base, harmonized, projected, scaled, lagged), names_to = "stage", values_to = "frontier") %>%
  filter(!is.na(frontier)) %>%
  mutate(stage = case_when(stage == "base" ~ "Base (10th percentile)",
                           stage == "lagged" ~ "Lagged (final)",
                           TRUE ~ capitalize(stage)),
         frontier = ifelse(frontier < 0.1, 0, frontier)) %>%
  mutate(stage = factor(stage, levels = c("Base (10th percentile)", "Harmonized", "Projected", "Scaled", "Lagged (final)")),
                        zero = frontier == 0)

colors <- colorFunct(n = length(unique(ggdata$stage)), names = levels(ggdata$stage), color.and.fill = TRUE)

grobs <- list()
for(i in unique(ggdata$causename)){

  ggdata2 <- ggdata %>% filter(causename == i)
  ggdata_points <- ggdata2 %>% filter(year >= 2000, year < 2020, stage != "Projected")
  ggdata_lines <- ggdata2 %>% filter(year > 2019, year <= 2040)

  id <- ids[which(unique(ggdata$causename) == i)]

  grobs[[i]] <- ggplot(ggdata2) +
    facet_wrap(~ age) +
    geom_line(data = ggdata_lines, aes(x = year, y = frontier, color = stage), size = 0.75) +
    geom_point(data = ggdata_points,
               aes(x = year, y = frontier, color = stage, fill = stage), pch = 21) +
    labs(title = paste(id, i)) +
    scale_x_continuous("Year", limits = c(1995, 2045),
                       breaks = seq(2000, 2040, 10), labels = c("2000", "'10", "'20", "'30", "'40")) +
    scale_y_continuous("Frontier mortality rate (per 100K, log scale)", trans = "log",
                       breaks = log.breaks, labels = log.labels) +
    scale_color_manual("Frontier estimation stage", values = colors$colors) +
    scale_fill_manual("Frontier estimation stage", values = colors$fills) +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(title.position = "bottom",
                                override.aes = list(pch = c(21, 21, NA, 21, 21))))

}

saveGGplot(x = grobs, name = "2-5_frontier-lagging_B.pdf", folder = "output/figures",
           width = 12, height = 10, multipage = TRUE)



# * 3.3 C Full process year facets ----------------------------------------

ggdata <- frontier_info %>%
  filter(age >= 30, is.na(sex) | sex == 2, year %in% seq(2000, 2040, 5)) %>%
  mutate(causename = ifelse(causename %in% sex.specific, paste(causename, "(females)"), causename)) %>%
  mutate(causename = factor(causename, levels = causename.levels2)) %>%
  pivot_longer(cols = c(base, harmonized, projected, scaled, lagged), names_to = "stage", values_to = "frontier") %>%
  filter(!is.na(frontier)) %>%
  mutate(stage = case_when(stage == "base" ~ "Base (10th percentile)",
                           stage == "lagged" ~ "Lagged (final)",
                           TRUE ~ capitalize(stage)),
         frontier = ifelse(frontier < 0.1, 0, frontier)) %>%
  mutate(stage = factor(stage, levels = c("Base (10th percentile)", "Harmonized", "Projected", "Scaled", "Lagged (final)")),
         zero = frontier == 0)

colors <- colorFunct(n = length(unique(ggdata$stage)), names = levels(ggdata$stage), color.and.fill = TRUE)
colors[["colors"]][3:5] <- colors[["fills"]][3:5]
shapes <- c(21, NA)

grobs <- list()
for(i in unique(ggdata$causename)){

  ggdata2 <- ggdata %>% filter(causename == i)
  ggdata_points <- ggdata2 %>% filter(year >= 2000, year < 2020, stage != "Projected")
  ggdata_lines <- ggdata2 %>% filter(year > 2019, year <= 2040)

  id <- ids[which(unique(ggdata$causename) == i)]

  grobs[[i]] <- ggplot(ggdata2) +
    facet_wrap(~ year) +
    geom_line(data = subset(ggdata2, stage %notin% c("Base (10th percentile)", "Harmonized")),
              aes(x = age, y = frontier, color = stage), size = 0.75) +
    geom_line(data = subset(ggdata2, stage %in% c("Base (10th percentile)", "Harmonized")),
              aes(x = age, y = frontier, color = stage), size = 0.75) +
    geom_point(data = subset(ggdata2, stage %in% c("Base (10th percentile)", "Harmonized")),
               aes(x = age, y = frontier, color = stage, fill = stage, shape = zero)) +
    labs(title = paste(id, i)) +
    scale_x_continuous("Age") +
    scale_y_continuous("Frontier mortality rate (per 100K, log scale)", trans = "log",
                       breaks = log.breaks, labels = log.labels) +
    scale_color_manual("Frontier estimation stage", values = colors$colors) +
    scale_fill_manual("Frontier estimation stage", values = colors$fills) +
    scale_shape_manual("", values = shapes) +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(title.position = "bottom",
                                override.aes = list(pch = c(21, 21, NA, NA, NA))),
           shape = "none")

}

saveGGplot(x = grobs, name = "2-5_frontier-lagging_C.pdf", folder = "output/figures",
           width = 12, height = 10, multipage = TRUE)



# 3.3 D Full process all data ---------------------------------------------

ggdata <- frontier_info %>%
  mutate(base2 = base) %>%
  pivot_longer(cols = c(base, harmonized, projected, scaled, lagged), names_to = "stage", values_to = "frontier") %>%
  filter(!is.na(frontier)) %>%
  mutate(stage = case_when(stage == "base" ~ "Base (10th percentile)",
                           stage == "lagged" ~ "Lagged (final)",
                           TRUE ~ capitalize(stage)),
         base2 = ifelse(base2 < 0.1, 0, base2),
         frontier = ifelse(frontier < 0.1, 0, frontier)) %>%
  mutate(stage = factor(stage, levels = c("Base (10th percentile)", "Harmonized", "Projected", "Scaled", "Lagged (final)"))) %>%
  filter(base2 != 0, frontier != 0, year >= 2000, year <= 2040) %>%
  arrange(stage == "Base (10th percentile)")

colors <- colorFunct(n = length(unique(ggdata$stage)), names = levels(ggdata$stage), color.and.fill = TRUE)

figure <- ggplot(ggdata) +
  geom_point(aes(x = base2, y = frontier, color = stage, fill = stage), pch = 21) +
  scale_x_continuous("Base frontier (log scale)", trans = "log", breaks = log.breaks, labels = log.labels) +
  scale_y_continuous("Modified frontier (log scale)", trans = "log", breaks = log.breaks, labels = log.labels) +
  scale_color_manual("Frontier estimation stage", values = colors$colors) +
  scale_fill_manual("Frontier estimation stage", values = colors$fills)

saveGGplot(x = figure, name = "2-5_frontier-lagging_D.pdf", folder = "output/figures",
           width = 10, height = 9)
