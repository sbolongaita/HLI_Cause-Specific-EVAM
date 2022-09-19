
### Healthy Longevity Initiative
### 620 - FIGURES

# This script creates figures for the main manuscript.



# 1 ENVIRONMENT -----------------------------------------------------------

# Tidying and loading environment
tidy(); sourceEnv()
scriptName <- "620-figures"

# Creating figures objects
folder <- makeFolder(folders[["figures"]])



# 2 Fig 1 mortality frontiers ---------------------------------------------

# Figure name
figure_name <- "Fig1.pdf"

# Loading data
sarahLoad(c("cause_hierarchy", "frontier_scaled"))

# Prepping data
ggdata <- frontier_scaled %>%
  filter(causename %in% c("Cardiovascular diseases", "Malignant neoplasms",
                          "Injuries", "All causes",
                          "Communicable, maternal, perinatal and nutritional conditions"),
         age >= 30) %>%
  mutate(frontier_log = ifelse(frontier < 0.000001, NA, frontier),
         prefix = getCauseInfo(ghecause, return = "prefix"),
         age2 = makeMathersAgeGroup(age),
         age = makeAgeGroup(age)) %>%
  arrange(prefix, age, sex, year)

color_info <- groupColorInfo(ggdata, "age2", "age")
colors <- groupColorFunct(color_info$n, color_info$names, color.and.fill = TRUE)

grobs <- list()
for(i in unique(ggdata$causename)){

  ggdata2 <- ggdata %>%
    filter(causename == i) %>%
    arrange(!is.na(frontier_log), year)
  id <- ids[which(unique(ggdata$causename) == i)]
  panels <- list()

  ylims <- ggRange(ggdata2$frontier)
  panels[["standard"]] <- ggplot(ggdata2) +
    facet_wrap(~ age2, nrow = 1) +
    geom_line(aes(x = year, y = frontier, color = age)) +
    geom_point(data = subset(ggdata2, year < 2020),
               aes(x = year, y = frontier, fill = age, color = age),
               pch = 21) +
    labs(title = paste(id, i),
         subtitle = "Standard scale") +
    scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("2000", "'10", "'20", "'30", "'40")) +
    scale_y_continuous("Frontier mortality rate (per 100K)", limits = ylims, labels = labelAuto) +
    scale_color_manual("", values = colors$colors) +
    scale_fill_manual("", values = colors$fills) +
    theme(axis.title.x = element_blank(),
          panel.background = element_rect(fill = NA)) +
    guides(color = guide_legend(nrow = 2))

  panels[["spacer"]] <- ""

  ylims <- ggRange(ggdata2$frontier_log, log = TRUE)
  panels[["log"]] <- ggplot(ggdata2) +
    facet_wrap(~ age2, nrow = 1) +
    geom_line(aes(x = year, y = frontier_log, color = age)) +
    geom_point(data = subset(ggdata2, year < 2020),
               aes(x = year, y = frontier_log, fill = age, color = age),
               pch = 21) +
    labs(subtitle = "Log scale") +
    scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("2000", "'10", "'20", "'30", "'40")) +
    scale_y_continuous("Frontier mortality rate (per 100K)", trans = "log",
                       limits = ylims, breaks = log.breaks, labels = log.labels) +
    scale_color_manual("", values = colors$colors) +
    scale_fill_manual("", values = colors$fills) +
    theme(axis.title.x = element_blank(),
          panel.background = element_rect(fill = NA)) +
    guides(color = guide_legend(nrow = 2))

  figure <- ggarrange(plotlist = panels, ncol = 1, heights = c(1, -0.1 , 1),
                             align = "hv", common.legend = TRUE, legend = "bottom")

  grobs[[id]] <- as_grob(figure)

}

saveMultipage(grobs, figure_name, width = 10, height = 8)



# 3 Fig 2 Latin America mortality trajectory ------------------------------

sarahLoad("region")

figure_name <- "Fig2.pdf"

ggdata <- region %>%
  filter(grepl("Latin America", region),
         causename %in% c("Cardiovascular diseases", "Malignant neoplasms",
                          "Injuries", "All causes",
                          "Communicable, maternal, perinatal and nutritional conditions"),
         age >= 30) %>%
  mutate(dths_rate_log = ifelse(dths_rate < 0.000001, NA, dths_rate),
         sex = case_when(sex == 1 ~ "Males",
                         sex == 2 ~ "Females",
                        TRUE ~ NA_character_),
         prefix = getCauseInfo(ghecause, return = "prefix"),
         age2 = makeMathersAgeGroup(age),
         age = makeAgeGroup(age)) %>%
  arrange(prefix, region, age, sex, year) %>%
  ungroup()

color_info <- groupColorInfo(ggdata, "age2", "age")
colors <- groupColorFunct(color_info$n, color_info$names, color.and.fill = TRUE)

grobs <- list()
for(i in unique(ggdata$causename)){

  ggdata2 <- ggdata %>% filter(causename == i)

  id <- ids[which(unique(ggdata$causename) == i)]

  if(all(ggdata2$dths_rate == 0)){
    ylims <- c(0, 3)
  }else{
    ylims <- ggRange(ggdata2$dths_rate)
  }

  if(all(is.na(ggdata2$dths_rate_log))){
    ylims_log <- log.breaks[c(5, 7)]
  }else{
    ylims_log <- ggRange(ggdata2$dths_rate_log, log = TRUE)
  }

  Panels <- list()
  for(j in unique(ggdata2$sex)){

    ggdata3 <- ggdata2 %>% filter(sex == j)
    panels <- list()

    title <- ifelse(j == "Females", paste(id, i), "")

    panels[["Standard"]] <- ggplot(ggdata3) +
      facet_wrap(~ age2, nrow = 1) +
      geom_line(aes(x = year, y = dths_rate, color = age)) +
      geom_point(data = subset(ggdata3, year < 2020), aes(x = year, y = dths_rate, fill = age, color = age),
                 pch = 21) +
      labs(title = title,
           subtitle = paste0(j, "   |   Standard scale")) +
      scale_x_continuous("", breaks = seq(2000, 2040, 20), labels = c("2000", "'20", "'40")) +
      scale_y_continuous("Mortality rate (per 100K)", limits = ylims, labels = labelAuto) +
      scale_color_manual("", values = colors$colors) +
      scale_fill_manual("", values = colors$fills) +
      theme(axis.title.x = element_blank(),
            panel.background = element_rect(fill = NA),
            legend.position = "bottom",
            panel.grid.minor.x = element_line(color = alpha("black", 0.2), size = 0.2)) +
      guides(color = guide_legend(nrow = 2))

    if(j == "Males"){
      panels[["Standard"]] <- panels[["Standard"]] + theme(axis.title.y = element_blank())
    }

    panels[[paste(j, "Spacer")]] <- ""

    panels[["Log"]] <- ggplot(ggdata3) +
      facet_wrap(~ age2, nrow = 1) +
      geom_line(aes(x = year, y = dths_rate_log, color = age)) +
      geom_point(data = subset(ggdata3, year < 2020), aes(x = year, y = dths_rate_log, fill = age, color = age),
                 pch = 21) +
      labs(subtitle = paste0(j, "   |   Log scale")) +
      scale_x_continuous("", breaks = seq(2000, 2040, 20), labels = c("2000", "'20", "'40")) +
      scale_y_continuous("Mortality rate (per 100K)", trans = "log",
                         limits = ylims_log, breaks = log.breaks, labels = log.labels) +
      scale_color_manual("", values = colors$colors) +
      scale_fill_manual("", values = colors$fills) +
      theme(axis.title.x = element_blank(),
            panel.background = element_rect(fill = NA),
            panel.grid.minor.x = element_line(color = alpha("black", 0.2), size = 0.2))

    if(j == "Males"){
      panels[["Log"]] <- panels[["Log"]] + theme(axis.title.y = element_blank())
    }

    Panels[[j]] <- ggarrange(plotlist = panels, ncol = 1, heights = c(1, -0.1, 1),
                             align = "hv", legend = "none")

  }

  figure <- ggarrange(plotlist = Panels, ncol = 2, align = "hv", widths = c(1.05, 1),
                      common.legend = TRUE, legend = "bottom", legend.grob = get_legend(panels[[1]]))

  grobs[[id]] <- as_grob(figure)

}

saveMultipage(grobs, figure_name, width = 13, height = 8)



# 4 Fig 3 R bar -----------------------------------------------------------

sarahLoad("region_calculations")

figure_name <- "Fig3.pdf"

ggdata <- region_calculations %>%
  filter(mece == "Level 2", lambda == 5, epsilon == 1,
         year %in% c(2019, 2030, 2040)) %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females"),
         main_causename = getCauseInfo(ghecause, return = "main_causename"),
         main_causename = factor(main_causename, levels = c("Communicable, maternal, perinatal and nutritional conditions",
                                                            "Noncommunicable diseases", "Injuries")),
         causename = factor(wrapper(causename, 35),
                            levels = wrapper(causename.levels, 35))) %>%
  arrange(region, sex, mece, main_causename, causename, year) %>%
  ungroup()

panels <- list()
for(i in unique(ggdata$sex)){

  ggdata2 <- ggdata %>% filter(sex == i)
  id <- ids[which(unique(ggdata$sex == i))]

  names <- unique(ggdata2$causename)
  greys <- ggsci::pal_material("grey")(9)[c(3, 5, 9)]
  fills <- c(greys[3], colorFunct(7), greys[2], greys[1])
  colors <- darken(fills, 0.3)
  colors <- list(colors = setNames(colors, names),
                 fills = setNames(fills, names))

  panels[[id]] <- ggplot(ggdata2) +
    facet_wrap(~ wrapper(region, 20), nrow = 1) +
    geom_col(aes(x = as.factor(year), y = R_bar, fill = causename, color = causename)) +
    labs(title = paste(id, i)) +
    scale_x_discrete("") +
    scale_y_continuous(bquote(bar(R)), expand = expansion(mult = c(0, 0.05))) +
    scale_color_manual("Level 2 causes", values = colors$colors) +
    scale_fill_manual("Level 2 causes", values = alpha(colors$fills, 0.9)) +
    theme(axis.line = element_blank(), axis.ticks.x = element_blank(),
          legend.key.height = unit(25, "pt"), legend.position = "bottom",
          panel.grid.major.x = element_blank()) +
    guides(color = guide_legend(ncol = 1, title.position = "top"))

  if(i == "Females"){
    panels[[id]] <- panels[[id]] + theme(axis.title.x = element_blank())
  }

}

figure <- ggarrange(plotlist = panels, ncol = 1, align = "hv", common.legend = TRUE, legend = "right") %>%
  annotate_figure(bottom = text_grob(bquote("Note: The displayed"~bar(R)~"estimates are calculated using an income elasticity of 1."),
                                     family = "Barlow", x = 0.06, hjust = 0, vjust = 0))

saveGGplot(figure, figure_name, width = 14, height = 10)



# 5 Fig 4 economic values -------------------------------------------------

figure_name <- "Fig4.pdf"

sarahLoad("region_calculations")

region.levels <- rev(sort(unique(region_calculations$region)))

ggdata <- region_calculations %>%
  filter(ghecause %in% c(0, 10, 1100, 610, 1510),
         year == 2019, lambda == 5, epsilon == 1, mece %in% c("Level 1", "Level 2")) %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females"),
         sex = factor(sex, levels = c("Males", "Females")),
         causename = factor(wrapper(causename, 35), levels = rev(wrapper(causename.levels, 35))),
         multiple = V_bar / Y) %>%
  group_by(region, year, sex, ghecause, causename) %>%
  slice_head(n = 1) %>%
  dplyr::select(region, year, sex, ghecause, causename, multiple) %>%
  group_by(region, year, ghecause, causename) %>%
  mutate(lower = min(multiple),
         upper = max(multiple)) %>%
  ungroup()

ggdata_segments <- ggdata %>%
  select(region, year, ghecause, causename, lower, upper) %>%
  unique()

colors <- colorFunct(length(unique(ggdata$causename)),
                     unique(ggdata$causename), color.and.fill = TRUE)
shapes <- c("Females" = 21, "Males" = 24)

figure <- ggplot(ggdata) +
  facet_wrap(~ region, ncol = 1) +
  geom_segment(data = ggdata_segments,
               aes(y = causename, yend = causename, x = lower, xend = upper, color = causename),
               size = 0.75, alpha = 0.5) +
  geom_point(aes(x = multiple, y = causename, color = causename, fill = causename, shape = sex), size = 2) +
  geom_label(data = ggdata_segments, aes(x = lower-0.04, y = causename, label = format(round(lower, 2), nsmall = 2, trim = TRUE)),
             hjust = 1, family = "Barlow", fill = alpha("white", 0.8), label.size = 0) +
  geom_label(data = ggdata_segments, aes(x = upper+0.04, y = causename, label = format(round(upper, 2), nsmall = 2, trim = TRUE)),
             hjust = 0, family = "Barlow", fill = "white", label.size = 0) +
  labs(caption = bquote("Note: Economic values are calculated using an income elasticity of 1.")) +
  scale_x_continuous("Multiples of GNI per capita", expand = expansion(mult = c(0.1, 0.1)),
                     breaks = pretty_breaks(5)) +
  scale_y_discrete("") +
  scale_color_manual("", values = colors$colors) +
  scale_fill_manual("", values = colors$fills) +
  scale_shape_manual("", values = shapes) +
  theme(axis.title.y = element_blank(), axis.line.y = element_blank(),
        legend.position = "bottom",
        panel.grid.minor.x = element_line(color = alpha("black", 0.2), size = 0.2)) +
  guides(color = "none", fill = "none",
         shape = guide_legend(override.aes = list(shape = c(19, 17))))

saveGGplot(figure, figure.name = figure_name, width = 10, height = 12)



# 5 END -------------------------------------------------------------------

# Tidying environment and notifying the end of the script
notifyScript(); tidy()
