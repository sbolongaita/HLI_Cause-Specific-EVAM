
### 5.4 Appendix figures



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()


sarahLoad(c("frontier_lagged", "region"), folder = "data/processed")

temp1 <- region %>%
  filter(year <= 2040, age >= 30) %>%
  select(-c(dths, pop))

temp2 <- temp1 %>%
  select(year, sex, age, ghecause, causename) %>%
  unique() %>%
  mutate(sex_match = ifelse(causename %in% sex.specific, sex, NA),
         region = "Frontier") %>%
  select(region, year, sex_match, sex, everything())

temp3 <- frontier_lagged %>%
  filter(year >= 2000, age >= 30) %>%
  dplyr::rename(sex_match = sex,
                dths_rate = frontier)

temp4 <- full_join(temp2, temp3, by = c("year", "sex_match", "age", "ghecause", "causename")) %>%
  select(-sex_match)

temp5 <- bind_rows(temp1, temp4)

region.levels <- c(sort(unique(temp5$region[temp5$region != "Frontier"])), "Frontier")

ggdata <- temp5 %>%
  mutate(region = factor(region, levels = region.levels),
         age = makeAgeGroup(age),
         sex = ifelse(sex == 1, "Males", "Females"),
         dths_rate = ifelse(dths_rate < 0.1, 0, dths_rate)) %>%
  mutate(age = gsub(" ", "\n", age),
         zero = dths_rate == 0)

colors <- colorFunct(n = length(region.levels)-1, names = region.levels[1:length(region.levels)-1], color.and.fill = TRUE)
colors$colors <- c(colors$colors, "Frontier" = "black")
colors$fills <- c(colors$fills, "Frontier" = lighten("black", 0.3))
shapes <- c(21, NA)

grobs <- list()
for(i in unique(ggdata$causename)){

  panels <- list()
  for(j in unique(ggdata$sex)){

    ggdata2 <- ggdata %>% filter(causename == i, sex == j)

    ylims <- ggRange(ggdata %>% filter(causename == i) %>% pull(dths_rate))
    if(ylims[1] <= 0){
      ylims <- c(0.1, ylims[2])
    }

    id <- ids[which(unique(ggdata$causename) == i)]

    base <- ggplot(ggdata2) +
      facet_wrap(~ age, nrow = 3) +
      geom_line(data = subset(ggdata2, year >= 2020), aes(x = year, y = dths_rate, group = region, color = region)) +
      geom_point(data = subset(ggdata2, year < 2020), aes(x = year, y = dths_rate, group = region, color = region, fill = region, shape = zero)) +
      scale_x_continuous("Year", breaks = seq(2000, 2040, 10), labels = c("", "'10", "'20", "'30", "'40")) +
      scale_y_continuous("Mortality rate (per 100K)", trans = "log", limits = ylims, breaks = log.breaks, labels = log.labels) +
      scale_color_manual("Region", values = colors$colors) +
      scale_fill_manual("Region", values = colors$fills) +
      scale_shape_manual("", values = shapes) +
      guides(color = guide_legend(title.position = "bottom", ncol = 4, override.aes = list(shape = 21)),
             fill = guide_legend(title.position = "bottom", ncol = 4),
             shape = "none") +
      theme(legend.position = "bottom", legend.direction = "horizontal")

    if(j == "Males"){
      panels[[j]] <- base +
        labs(title = paste(id, i), subtitle = j)
    }else{
        panels[[j]] <- base +
          labs(title = "", subtitle = j)
    }

  }

  legend <- get_legend(base)

  grobs[[i]] <- ggarrange(plotlist = panels, ncol = 2, align = "hv",
                          common.legend = TRUE, legend = "bottom", legend.grob = legend)

}

saveGGplot(grobs, "region_frontier.pdf", folder = "output/figures",
           width = 15, height = 12, multipage = TRUE)



# 2 Fig A1 comparison of frontier definitions ------------------------------

sarahLoad("frontier_comparison")

figure_name <- "FigA1.pdf"

frontier_comparison$prefix <- getCauseInfo(frontier_comparison$ghecause, return = "prefix")
frontier_comparison$level <- getCauseInfo(frontier_comparison$ghecause, return = "level")
ggdata <- frontier_comparison %>%
  filter(is.na(sex) | sex == 2, age >= 30) %>%
  mutate(age2 = makeMathersAgeGroup(age),
         causename = ifelse(is.na(sex), causename, paste(causename, "(females)"))) %>%
  mutate(age = makeAgeGroup(age)) %>%
  pivot_longer(cols = c("min", "p10"), names_to = "definition", values_to = "frontier") %>%
  mutate(definition = ifelse(definition == "min", "Minimum", "10th percentile"),
         frontier = ifelse(frontier < 0.00001, NA, frontier)) %>%
  arrange(prefix, causename, age, desc(definition), is.na(frontier))

shapes <- c("10th percentile" = 16, "Minimum" = 21)
lines <- c("10th percentile" = 1, "Minimum" = 1)
color_info <- ggdata %>%
  dplyr::select(age2, age) %>% unique() %>%
  group_by(age2) %>%
  dplyr::summarize(n = n(), age = paste0(age, collapse = "; "))
colors <- groupColorFunct(n = color_info$n,
                            names = unlist(str_split(paste(color_info$age, collapse = "; "), "; ")), color.and.fill = TRUE)

grobs <- list()
for(i in unique(ggdata$causename)){

  id <- ids[which(unique(ggdata$causename) == i)]

  ggdata2 <- ggdata %>% filter(causename == i)

  figure <- ggplot(ggdata2) +
    facet_wrap(~ age, ncol = 6) +
    geom_path(aes(x = year, y = frontier, color = age, group = definition), linetype = "dotted") +
    geom_line(aes(x = year, y = frontier, color = age, linetype = definition)) +
    geom_point(aes(x = year, y = frontier, color = age, shape = definition), fill = "white") +
    labs(title = paste(id, i),
         subtitle = "Log scale*",
         caption = makeCaption(captions["Zero"])) +
    scale_x_continuous("", breaks = c(seq(2000, 2019, 10), 2019), labels = c("'00", "'10", "'19")) +
    scale_y_continuous("Frontier mortality rate (per 100K)", trans = "log",
                       breaks = log.breaks, labels = log.labels, limits = c(0.1, NA)) +
    scale_color_manual("Age", values = colors$colors) +
    scale_shape_manual("Frontier definition", values = shapes, breaks = c("10th percentile", "Minimum")) +
    scale_linetype_manual("Frontier definition", values = lines) +
    guides(color = "none",
           shape = guide_legend(title.position = "top"),
           linetype = guide_legend(title.position = "top")) +
    theme(axis.title.x = element_blank(),
          legend.position = "bottom",
          panel.grid.minor.x = element_line(color = alpha("black", 0.2), size = 0.2))

  grobs[[id]] <- as_grob(figure)

}

saveMultipage(grobs, figure_name, width = 10, height = 7)



# 3 Fig A2 longevity frontier scaling factors -----------------------------

sarahLoad("longevity_scaling_factors")

# Graphing scaling factors
figure_name <- "FigA2.pdf"

figure <- ggplot(longevity_scaling_factors) +
  geom_density(aes(x = sf)) +
  scale_x_continuous(expression(paste("Scaling factors (", gamma, ")"))) +
  scale_y_continuous("Density")

saveGGplot(figure, figure.name = figure_name, width = 7, height = 5)



# 4 Fig A3 frontier process -----------------------------------------------

sarahLoad(c("frontier_base", "frontier_harmonized", "frontier_projected", "frontier_scaled"))

frontier_base %<>% dplyr::rename(base = frontier)
frontier_harmonized %<>% dplyr::rename(harmonized = frontier)
frontier_projected %<>%
  mutate(projected = ifelse(!is.na(frontier), frontier, projection)) %>%
  dplyr::select(-c(frontier, projection))
frontier_scaled %<>% dplyr::rename(scaled = frontier)

data <- full_join(frontier_base, frontier_harmonized) %>%
  full_join(frontier_projected) %>% full_join(frontier_scaled)

temp1 <- data %>%
  filter(age >= 30) %>%
  mutate(causename = case_when(sex == 1 ~ paste0(causename, " (males)"),
                               sex == 2 ~ paste0(causename, " (females)"),
                               TRUE ~ causename)) %>%
  pivot_longer(cols = c(base, harmonized, projected, scaled), names_to = "stage", values_to = "frontier") %>%
  mutate(stage = capitalize(stage))

ggdata <- temp1 %>%
  filter(is.na(sex) | sex == 2) %>%
  mutate(age2 = makeMathersAgeGroup(age),
         age = makeAgeGroup(age),
         frontier_log = ifelse(frontier < 0.001, NA, frontier))

color.info <- groupColorInfo(ggdata, "age2", "age")
colors <- groupColorFunct(color.info$n, color.info$names, color.and.fill = TRUE)

grobs.standard <- list()
grobs.log <- list()
for(i in unique(ggdata$causename)){

  ggdata2 <- ggdata %>% filter(causename == i)
  id <- ids[which(unique(ggdata$causename) == i)]

  # panels[["standard"]] <- ggplot(ggdata2) +
  #   facet_grid(cols = vars(stage), rows = vars(age2)) +
  #   geom_point(data = subset(ggdata2, year < 2020),
  #              aes(x = year, y = frontier, color = age, fill = age), pch = 21) +
  #   geom_line(data = subset(ggdata2, year >= 2020),
  #             aes(x = year, y = frontier, color = age)) +
  #   labs(title = paste(id, i), subtitle = "Standard scale") +
  #   scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
  #   scale_y_continuous("Frontier mortality rate (per 100K)", breaks = pretty_breaks(3), labels = labelAuto) +
  #   scale_color_manual("", values = colors$colors) +
  #   scale_fill_manual("", values = colors$fills) +
  #   theme(axis.line = element_blank(), axis.title.x = element_blank(),
  #         legend.position = "bottom",
  #         panel.border = element_rect(color = alpha("black", 0.8), size = 0.2))
  # grobs.standard[[id]] <- as_grob(panels[["standard"]])

  figure <- ggplot(ggdata2) +
    facet_grid(cols = vars(stage), rows = vars(age2)) +
    geom_point(data = subset(ggdata2, year < 2020),
               aes(x = year, y = frontier_log, color = age, fill = age), pch = 21) +
    geom_line(data = subset(ggdata2, year >= 2020),
              aes(x = year, y = frontier_log, color = age)) +
    labs(title = paste(id, i), subtitle = "Log scale*",
         caption = makeCaption(captions["Zero"])) +
    scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("2000", "'10", "'20", "'30", "'40")) +
    scale_y_continuous("Frontier mortality rate (per 100K)", trans = "log", breaks = log.breaks, labels = log.labels) +
    scale_color_manual("", values = colors$colors) +
    scale_fill_manual("", values = colors$fills) +
    theme(axis.line = element_blank(), axis.title.x = element_blank(),
          legend.position = "bottom",
          panel.border = element_rect(color = alpha("black", 0.8), size = 0.2)) +
    guides(color = guide_legend(nrow = 2),
           fill = guide_legend(nrow = 2))
  grobs.log[[id]] <- as_grob(figure)

}

# saveMultipage(grobs.standard, figure.name = "FigA3_standard.pdf", width = 12, height = 10)
saveMultipage(grobs.log, figure.name = "FigA3.pdf", width = 12, height = 10)



# 5 Fig A4 modulating function --------------------------------------------

figure_name <- "FigA4.pdf"

ggdata <- expand_grid(mrr = seq(0, 1, length.out = 100),
                      lambda = c(1, 5, 10)) %>%
  mutate(`F` = (1-exp(-1*lambda*mrr))/(1-exp(-1*lambda)))

colors <- colorFunct(n = 3, names = c("1", "5", "10"))

figure <- ggplot(ggdata) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = alpha("black", 0.3)) +
  geom_line(aes(x = mrr, y = `F`, color = as.factor(lambda), group = as.factor(lambda))) +
  scale_x_continuous(bquote("Mortality risk reduction ("*delta*")")) +
  scale_color_manual(bquote(lambda), values = colors)

saveGGplot(figure, figure.name = figure_name, width = 7.5, height = 5)



# 6 Fig B1 scaled frontier projections ------------------------------------

sarahLoad("frontier_scaled")

figure_name <- "FigB1.pdf"

ggdata <- frontier_scaled %>%
  filter(is.na(sex) | sex == 2, age >= 30) %>%
  mutate(causename = case_when(sex == 1 ~ paste(causename, "(males)"),
                               sex == 2 ~ paste(causename, "(females)"),
                               TRUE ~ causename),
         frontier_log = ifelse(frontier < 0.000001, NA, frontier),
         prefix = getCauseInfo(ghecause, return = "prefix"),
         prefix = case_when(sex == 1 ~ paste0(prefix, "M"),
                            sex == 2 ~ paste0(prefix, "F"),
                            TRUE ~ prefix),
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

  if(all(ggdata2$frontier == 0)){
    ylims <- c(0, 3)
  }else{
    ylims <- ggRange(ggdata2$frontier)
  }

  panels[["standard"]] <- ggplot(ggdata2) +
    facet_wrap(~ age2, nrow = 1) +
    geom_line(data = subset(ggdata2, year >= 2020), aes(x = year, y = frontier, color = age)) +
    geom_point(data = subset(ggdata2, year < 2020), aes(x = year, y = frontier, fill = age, color = age),
               pch = 21) +
    labs(title = paste(id, i),
         subtitle = "Standard scale") +
    scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("2000", "'10", "'20", "'30", "'40")) +
    scale_y_continuous("Frontier mortality rate (per 100K)", limits = ylims, labels = labelAuto) +
    scale_color_manual("", values = colors$colors) +
    scale_fill_manual("", values = colors$fills) +
    theme(axis.title.x = element_blank(),
          panel.background = element_rect(fill = NA)) +
    guides(color = guide_legend(nrow = 2),
           fill = guide_legend(nrow = 2))

  panels[["spacer"]] <- ""

  if(all(is.na(ggdata2$frontier_log))){
    ylims <- log.breaks[c(5, 7)]
    }else{
      ylims <- ggRange(ggdata2$frontier_log, log = TRUE)
    }

  panels[["log"]] <- ggplot(ggdata2) +
    facet_wrap(~ age2, nrow = 1) +
    geom_line(data = subset(ggdata2, year >= 2020), aes(x = year, y = frontier_log, color = age)) +
    geom_point(data = subset(ggdata2, year < 2020), aes(x = year, y = frontier_log, fill = age, color = age),
               pch = 21) +
    labs(subtitle = "Log scale*") +
    scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("2000", "'10", "'20", "'30", "'40")) +
    scale_y_continuous("Frontier mortality rate (per 100K)", trans = "log",
                       limits = ylims, breaks = log.breaks, labels = log.labels) +
    scale_color_manual("", values = colors$colors) +
    scale_fill_manual("", values = colors$fills) +
    theme(axis.title.x = element_blank(),
          panel.background = element_rect(fill = NA)) +
    guides(color = guide_legend(nrow = 2),
           fill = guide_legend(nrow = 2))

  figure <- ggarrange(plotlist = panels, ncol = 1, heights = c(1, -0.1 , 1),
                                      align = "hv", common.legend = TRUE, legend = "bottom") %>%
    annotate_figure(bottom = text_grob(makeCaption(captions["Zero"]),
                                       family = "Barlow", size = 11, hjust = 0, x = 0.075))

  grobs[[id]] <- as_grob(figure)

}

saveMultipage(grobs, figure_name, width = 10, height = 8)



# 7 Fig B2-B7 Regional mortality projections ------------------------------

sarahLoad("region")

region$prefix <- getCauseInfo(region$ghecause, return = "prefix")
ggdata <- region %>%
  filter(age >= 30) %>%
  mutate(dths_rate_log = ifelse(dths_rate < 0.001, NA, dths_rate),
         age2 = makeMathersAgeGroup(age),
         age = makeAgeGroup(age)) %>%
  arrange(prefix, region, age, sex, year) %>%
  ungroup()

color.info <- groupColorInfo(ggdata, "age2", "age")
colors <- groupColorFunct(color.info$n, color.info$names, color.and.fill = TRUE)

for(j in unique(ggdata$region)){

  grobs <- list()

  for(i in unique(ggdata$causename)){

    ggdata2 <- ggdata %>% filter(region == j, causename == i)
    id <- ids[which(unique(ggdata$causename) == i)]
    ylims <- ggRange(ggdata2$dths_rate); ylims.log <- ggRange(ggdata2$dths_rate_log, log = TRUE)

    Panels <- list()
    for(k in unique(ggdata2$sex)){

      panels <- list()
      ggdata3 <- ggdata2 %>% filter(sex == k)
      title <- ifelse(k == 2, paste(id, i), " ")
      sex <- ifelse(k == 1, "Males", "Females")

      # Standard scale
      panels[["standard"]] <- ggplot(ggdata3) +
        facet_grid(cols = vars(age2)) +
        geom_line(data = subset(ggdata3, year >= 2020),
                  aes(x = year, y = dths_rate, color = age), size = 0.5) +
        geom_point(data = subset(ggdata3, year < 2020),
                   aes(x = year, y = dths_rate, color = age, fill = age)) +
        labs(title = title,
             subtitle = paste0(sex, "   |   Standard scale")) +
        scale_x_continuous("", breaks = seq(2000, 2040, 20), labels = c("2000", "'20", "'40")) +
        scale_y_continuous("Mortality rate (per 100K)", limits = ylims, breaks = pretty_breaks(5),
                           labels = labelAuto) +
        scale_color_manual("", values = colors$colors) +
        scale_fill_manual("", values = colors$fills) +
        theme(axis.title.x = element_blank(), legend.position = "bottom",
              panel.grid.minor.y = element_blank()) +
        guides(color = guide_legend(nrow = 2),
               fill = guide_legend(nrow = 2))

      if(k == 1){
        panels[["standard"]] <- panels[["standard"]] + theme(axis.title.y = element_blank())
      }

      # Spacer
      panels[["space"]] <- ""

      # Log scale
      panels[["log"]] <- ggplot(ggdata3) +
        facet_grid(cols = vars(age2)) +
        geom_line(data = subset(ggdata3, year >= 2020),
                  aes(x = year, y = dths_rate, color = age), size = 0.5) +
        geom_point(data = subset(ggdata3, year < 2020),
                   aes(x = year, y = dths_rate_log, color = age, fill = age), pch = 21) +
        labs(subtitle = paste0(sex, "   |   Log scale*")) +
        scale_x_continuous("", breaks = seq(2000, 2040, 20), labels = c("2000", "'20", "'40")) +
        scale_y_continuous("Mortality rate (per 100K)", trans = "log", limits = ylims.log,
                           breaks = log.breaks, labels = log.labels) +
        scale_color_manual("", values = colors$colors) +
        scale_fill_manual("", values = colors$fills) +
        theme(axis.title.x = element_blank(), legend.position = "bottom",
              panel.grid.minor.y = element_blank()) +
        guides(color = guide_legend(nrow = 2),
               fill = guide_legend(nrow = 2))

      if(k == 1){
        panels[["log"]] <- panels[["log"]] + theme(axis.title.y = element_blank())
      }

      Panels[[sex]] <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.1, 1),
                                 common.legend = TRUE, legend = "none")

    }

    figure <- ggarrange(plotlist = Panels[c(2, 1)], ncol = 2, align = "hv", widths = c(1.05, 1),
                        common.legend = TRUE, legend = "bottom", legend.grob = get_legend(panels[[1]])) %>%
      annotate_figure(bottom = text_grob(makeCaption(captions["Zero"]),
                                         family = "Barlow", size = 11, hjust = 0, x = 0.075))

    grobs[[i]] <- as_grob(figure)

  }

  fig.num <- which(unique(ggdata$region) == j) + 1
  saveMultipage(grobs, paste0("FigB", fig.num, ".pdf"), width = 13, height = 8)
  print(paste(j, paste0("FigB", fig.num, ".pdf")))

}



# 8 Fig B8 R bar -----------------------------------------------------------

sarahLoad(c("cause_hierarchy", "region_calculations"))

figure_name <- "FigB8.pdf"

ggdata <- region_calculations %>%
  left_join(cause_hierarchy, by = c("ghecause", "causename")) %>%
  filter(lambda == 5, epsilon == 1,
         year %in% c(2019, 2030, 2040)) %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females"),
         main_causename = getCauseInfo(ghecause, return = "main_causename"),
         causename = factor(wrapper(causename, 35), levels = wrapper(causename.levels, 35))) %>%
  mutate(main_causename = factor(main_causename, levels = c("Communicable, maternal, perinatal and nutritional conditions",
                                                            "Noncommunicable diseases", "Injuries"))) %>%
  arrange(region, sex, mece, main_causename, causename, year) %>%
  ungroup()

Mece <- c("mece_lvl1", "mece_lvl3")
grobs <- list()
for(j in Mece){

  id <- ids[which(Mece == j)]

  panels <- list()
  for(i in unique(ggdata$sex)){

    Level <- gsub("[a-z_]", "", j)
    ggdata2 <- ggdata %>%
      filter(!!as.name(j) == TRUE, grepl(Level, mece), sex == i)

    title <- ifelse(i == "Females", paste(id, "Level", Level, "causes of death"), " ")
    color_info <- groupColorInfo(ggdata2, "main_causename", "causename")
    if(color_info$n[3] == 1){
      greys <- c(ggsci::pal_material("grey")(9)[c(9, 5)])
    }else{
      greys <- c(ggsci::pal_material("grey")(9)[c(9, 7, 5, 3)])
    }
    fills <- setNames(c(greys[1], colorFunct(color_info$n[2]), greys[-1]), color_info$names)
    colors <- setNames(darken(fills, 0.3), color_info$names)

    panels[[i]] <- ggplot(ggdata2) +
      facet_wrap(~ wrapper(region, 15), nrow = 1) +
      geom_col(aes(x = as.factor(year), y = R_bar, fill = causename, color = causename)) +
      labs(title = title, subtitle = i) +
      scale_x_discrete("") +
      scale_y_continuous(bquote(bar(R)), expand = expansion(mult = c(0, 0.05))) +
      scale_color_manual("", values = colors) +
      scale_fill_manual("", values = alpha(fills, 0.9)) +
      theme(axis.line = element_blank(), axis.ticks.x = element_blank(),
            legend.key.height = unit(26, "pt"), legend.position = "right",
            panel.grid.major.x = element_blank()) +
      guides(color = guide_legend(ncol = 1))

    if(i == "Females"){
      panels[[i]] <- panels[[i]] + theme(axis.title.x = element_blank())
      panels[["spacer"]] <- ""
    }

  }

  figure <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.1, 1),
                      common.legend = TRUE, legend = "right") %>%
    annotate_figure(bottom = text_grob(bquote("Note: The displayed"~bar(R)~"estimates are calculated using an income elasticity of 1."),
                                       family = "Barlow", x = 0.07, hjust = 0, vjust = 0))

  grobs[[id]] <- figure

}

saveMultipage(grobs, figure_name, width = 12, height = 10)



# 9 Fig B9 VSL ------------------------------------------------------------

sarahLoad("region_calculations")

ggdataA <- region_calculations %>% ungroup() %>%
  dplyr::select(region, epsilon, VSL) %>%
  unique() %>%
  mutate(region = wrapper(region, 15))

ggdataB <- ggdataA %>%
  filter(epsilon %in% c("0.8", "1.2")) %>%
  pivot_wider(id_cols = region, names_from = epsilon, values_from = "VSL") %>%
  dplyr::rename(start = `0.8`, stop = `1.2`)

colors <- colorFunct(length(unique(ggdataA$epsilon)), names = unique(ggdataA$epsilon),
                     color.and.fill = TRUE)

figure <- ggplot(ggdataA) +
  facet_wrap(~ region, nrow = 1, scales = "free_x") +
  geom_segment(data = ggdataB, aes(x = region, xend = region, y = start, yend = stop), size = 0.75) +
  geom_label(data = ggdataB, aes(x = region, y = start, label = paste0(round(start/1000000, 1), "M")),
             family = "Barlow", size = 4, hjust = 0, vjust = 0, nudge_x = 0.07,
             fill = "white", label.padding = unit(0.1, "lines"), label.size = 0) +
  geom_label(data = ggdataB, aes(x = region, y = stop, label = paste0(round(stop/1000000, 1), "M")),
             family = "Barlow", size = 4, hjust = 0, vjust = 1, nudge_x = 0.07,
             fill = "white", label.padding = unit(0.1, "lines"), label.size = 0) +
  geom_point(aes(x = region, y = VSL, color = as.factor(epsilon), fill = as.factor(epsilon)),
             pch = 21, size = 2) +
  scale_x_discrete("") +
  scale_y_continuous("International $, PPP", labels = label_number_si()) +
  scale_color_manual(bquote("Income elasticity ("*epsilon*")"), values = colors$colors) +
  scale_fill_manual(bquote("Income elasticity ("*epsilon*")"), values = colors$fills) +
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(),
        panel.border = element_rect(color = alpha("black", 0.25)), panel.grid.major.x = element_blank(),
        legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top"))

saveGGplot(figure, figure.name = "FigB9.pdf", height = 6, width = 10)




# 10 Fig C1 lambda ---------------------------------------------------------

# Figure base name
figure_name <- "FigC1.pdf"

sarahLoad("region_calculations")

# Prepping data
ggdata <- region_calculations %>%
  left_join(cause_hierarchy, by = c("ghecause", "causename")) %>%
  filter(year %in% c(2019, 2030, 2040), epsilon == 1, mece_lvl2, mece == "Level 2") %>%
  mutate(year = as.factor(year),
         sex = ifelse(sex == 1, "Males", "Females"),
         causename = factor(wrapper(causename, 35), wrapper(causename.levels, 35))) %>%
  mutate(main_causename = factor(main_causename, unique(cause_hierarchy$main_causename)[!is.na(unique(cause_hierarchy$main_causename))])) %>%
  arrange(main_causename, prefix)

# Graphing parm
color_info <- groupColorInfo(ggdata2, "main_causename", "causename")
greys <- c(ggsci::pal_material("grey")(9)[c(9, 7, 5, 3)])
fills <- setNames(c(greys[1], colorFunct(color_info$n[2]), greys[-1]), color_info$names)
colors <- setNames(darken(fills, 0.3), color_info$names)

# Graphing
grobs <- list()
for(i in unique(ggdata$region)){

  id <- ids[which(unique(ggdata$region) == i)]

  panels <- list()
  for(j in rev(unique(ggdata$sex))){

    ggdata2 <- ggdata %>% filter(region == i, sex == j)
    title <- ifelse(j == "Females", paste(id, i), "")

    panels[[j]] <- ggplot(ggdata2) +
      facet_wrap(~ year) +
      geom_bar(aes(x = as.factor(lambda), y = R, fill = causename, color = causename),
               position = "fill", stat = "identity") +
      labs(title = title,
           subtitle = j) +
      scale_x_discrete(bquote(lambda)) +
      scale_y_continuous(bquote(bar(R)), expand = expansion(mult = c(0, 0.025))) +
      scale_color_manual("Level 2 causes", values = colors, breaks = wrapper(causename.levels, 35)) +
      scale_fill_manual("Level 2 causes", values = alpha(fills, 0.9), breaks = wrapper(causename.levels, 35)) +
      theme(axis.line = element_blank(), axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.key.height = unit(22, "pt"))

    if(j == "Females"){
      panels[["Spacer"]] <- ""
    }

  }

  figure <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1.05, -0.13 , 1),
                      common.legend = TRUE, legend = "right") %>%
    annotate_figure(bottom = text_grob(bquote("Note: The displayed"~bar(R)~"estimates are calculated using an income elasticity of 1."),
                                       family = "Barlow", x = 0.08, hjust = 0, vjust = 0))

  grobs[[id]] <- as_grob(figure)

}

saveMultipage(grobs, figure.name = "FigC1.pdf", width = 11, height = 9)



# 11 END ------------------------------------------------------------------

# Tidying environment and notifying the end of the script
notifyScript(); tidy()
