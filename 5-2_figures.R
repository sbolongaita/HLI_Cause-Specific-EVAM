
### 5.2 Figures

source("config/init.R")



# 1 Fig 1 LAC vs frontier -------------------------------------------------

applyEnv()

figure_name <- "Fig1_latin-america-caribbean-vs-frontier.pdf"

sarahLoad(c("cause_hierarchy", "frontier_scaled", "region"), folder = "data/processed")

temp1 <- region %>%
  filter(region == "Latin America & Caribbean", year >= 2000, year <= 2050, age >= 20, causename %in% focus) %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females"),
         age2 = makeDisplayAgeGroup(age),
         age = makeAgeGroup(age),
         causename = factor(causename, levels = focus)) %>%
  group_by(region, year, sex, age2, ghecause, causename) %>%
  summarize_at(vars(dths, pop), ~sum(.), .groups = "drop") %>%
  mutate(dths_rate = dths / pop * 100000) %>%
  ungroup() %>%
  select(year, sex.frontier = sex, age2, causename, dths_rate)

temp2 <- frontier_scaled %>%
  filter(year >= 2000, year <= 2050, age >= 20, causename %in% focus) %>%
  mutate(sex.frontier = "Frontier",
         age2 = makeDisplayAgeGroup(age),
         age = makeAgeGroup(age),
         causename = factor(causename, levels = focus)) %>%
  group_by(year, sex, age2, ghecause, causename) %>%
  mutate(frontier = mean(frontier)) %>%
  ungroup() %>%
  select(year, sex.frontier, age2, causename, dths_rate = frontier)

ggdata <- bind_rows(temp1, temp2) %>%
  mutate(sex.frontier = factor(sex.frontier, levels = c("Females", "Males", "Frontier")),
         dths_rate = ifelse(year %in% covid, NA, dths_rate)) %>%
  arrange(year, age2, causename, rev(sex.frontier))

fills <- setNames(colorFunct(length(levels(ggdata$sex.frontier))), levels(ggdata$sex.frontier))
colors <- setNames(darken(fills, 0.3), levels(ggdata$sex.frontier))

panels <- list()
for(i in unique(ggdata$causename)){

  ggdata2 <- ggdata %>% filter(causename == i)

  ylims <- ggRange(ggdata2$dths_rate)
  if(ylims[1] <= 0){
    ylims <- c(0.1, ylims[2])
  }
  ylab <- ifelse(i == "Cardiovascular diseases", "Mortality rate (per 100K, log scale)", "")

  panels[[i]] <- ggplot(ggdata2) +
    facet_wrap(~ age2, nrow = 1) +
    geom_line(aes(x = year, y = dths_rate, color = sex.frontier, group = sex.frontier), alpha = 0.3) +
    geom_line(data = subset(ggdata2, year >= 2020),
              aes(x = year, y = dths_rate, color = sex.frontier, group = sex.frontier)) +
    geom_point(data = subset(ggdata2, year < 2020),
               aes(x = year, y = dths_rate, color = sex.frontier, fill = sex.frontier),
               shape = 21, size = 1) +
    labs(title = i) +
    scale_x_continuous("", breaks = seq(2000, 2050, 10), labels = c("'00", "'10", "'20", "'30", "'40", "'50"),
                       expand = expansion(mult = 0.1)) +
    scale_y_continuous(ylab, trans = "log",
                       limits = ylims, breaks = log.breaks, labels = log.labels) +
    scale_color_manual("", values = colors) +
    scale_fill_manual("", values = fills) +
    theme(axis.title.x = element_blank(),
          legend.position = "bottom")

  if(i != tail(unique(ggdata$causename), 1)){
    panels[[paste(i, "spacer")]] <- ""
  }

  if(i == tail(unique(ggdata$causename), 1)){
    grob <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.025, 1, -0.025, 1, -0.025, 1, -0.025, 1),
                      common.legend = TRUE, legend = "bottom")
    saveGGplot(grob, figure_name, "output/figures", width = 9, height = 12)
  }

}


# 2 Fig 2 Economic value --------------------------------------------------

applyEnv()

figure_name <- "Fig2_economic-value.pdf"

sarahLoad(c("cause_hierarchy", "cause_aesth"), folder = "data/processed")
sarahLoad("region_calculations", folder = "output/data")

ggdata <- region_calculations %>%
  filter(mece.lvl == 2, sex != 3) %>%
  left_join(cause_hierarchy %>% select(ghecause, main_causename), by = "ghecause") %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females")) %>%
  mutate(sex = factor(sex, levels = c("Females", "Males"))) %>%
  ungroup() %>%
  arrange(region, year, sex, mece.lvl, ghecause)

panels <- list()
for(j in unique(ggdata$sex)){

  ggdata2 <- ggdata %>% filter(sex == j) %>%
    left_join(cause_aesth, by = c("ghecause", "causename", "mece.lvl")) %>%
    mutate(causename = factor(causename, levels = causename.levels))

  aesth <- ggdata2 %>% select(causename, color) %>% unique()
  fills <- setNames(aesth$color, aesth$causename)
  colors <- setNames(darken(aesth$color, 0.3), aesth$causename)

  breaks <- causename.levels[causename.levels %in% unique(ggdata2$causename)]

  panels[[j]] <- ggplot(ggdata2) +
    facet_wrap(~ wrapper(region, 15), nrow = 1) +
    geom_col(aes(x = as.factor(year), y = v.r, fill = causename, color = causename)) +
    labs(title = j) +
    scale_x_discrete("") +
    scale_y_continuous("Proportion of annual income", limits = c(0, 0.5), expand = expansion(mult = c(0, 0.03))) +
    scale_color_manual("Level 2 causes", values = colors, breaks = breaks) +
    scale_fill_manual("Level 2 causes", values = fills, breaks = breaks) +
    theme(axis.line = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(),
          legend.position = "right",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_line(color = alpha("black", 0.2), size = 0.2)) +
    guides(color = guide_legend(ncol = 1, title.position = "top"))

  if(j == "Females"){
    panels[["spacer"]] <- ""
  }

  if(j == "Males"){
    grob <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.01, 1),
                      common.legend = TRUE, legend = "right")
    saveGGplot(grob, figure_name, "output/figures", width = 13, height = 12)
  }

}



# 3 Fig 3 ROC -------------------------------------------------------------

applyEnv()

# Figure name
figure_name <- "Fig3_roc.pdf"

sarahLoad(c("cause_hierarchy", "cause_aesth"), folder = "data/processed")
sarahLoad("roc", folder = "output/data")

wrap <- 35

ggdata <- roc %>% ungroup() %>%
  filter(region != "World", sex != 3) %>%
  left_join(cause_hierarchy %>% select(ghecause, main_causename), by = "ghecause") %>%
  filter((main_causename == "Noncommunicable diseases" & mece.lvl == 2) |
         (main_causename != "Noncommunicable diseases" & mece.lvl == 1)) %>%
  left_join(cause_aesth, by = c("ghecause", "causename", "mece.lvl")) %>%
  mutate(causename = factor(wrapper(causename, wrap), levels = wrapper(causename.levels, wrap)),
         sex = ifelse(sex == 1, "Males", "Females")) %>%
  arrange(causename)

bound <- max(abs(ggRange(c(ggdata$roc1, ggdata$roc2), 0.01)))

steady.decline <- data.frame(x = c(-bound, -bound, 0, 0), y = c(-bound, 0, 0, -bound), label = "Steady decline")
steady.increase <- data.frame(x = c(bound, bound, 0, 0), y = c(bound, 0, 0, bound), label = "Steady increase")
polys <- bind_rows(steady.decline, steady.increase)

aesth <- ggdata %>% select(causename, color) %>% unique()
fills <- setNames(aesth$color, aesth$causename)
colors <- setNames(darken(aesth$color, 0.5), aesth$causename)

grob <- ggplot(ggdata) +
  facet_wrap(~ region, ncol = 2) +
  geom_vline(aes(xintercept = 0), color = lighten("black", 0.3)) +
  geom_hline(aes(yintercept = 0), color = lighten("black", 0.3)) +
  geom_polygon(data = polys, aes(x = x, y = y, group = label), fill = alpha(ggsci::pal_material("grey")(9)[8], 0.2)) +
  geom_text(data = subset(polys, x == min(x) & y == min(y)),
            aes(x = x, y = y, label = label),
            hjust = 0, vjust = 0, nudge_x = 0.005, nudge_y = 0.005, family = "Barlow") +
  geom_text(data = subset(polys, x == max(x) & y == max(y)),
            aes(x = x, y = y, label = label),
            hjust = 1, vjust = 1, nudge_x = -0.005, nudge_y = -0.005, family = "Barlow") +
  geom_point(aes(x = roc1, y = roc2, color = causename, fill = causename, shape = sex), size = 4) +
  scale_x_continuous("Annual rate of change, 2000-2019\n(percent per year)", limits = c(-bound, bound), expand = expansion(mult = 0),
                     label = label_percent(), breaks = pretty_breaks(5)) +
  scale_y_continuous("Annual rate of change, 2019-2050\n(percent per year)", limits = c(-bound, bound), expand = expansion(mult = 0),
                     label = label_percent(), breaks = pretty_breaks(5)) +
  scale_color_manual("Cause", values = colors) +
  scale_fill_manual("Cause", values = fills) +
  scale_shape_manual("Sex", values = c(23, 22)) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.placement = "outside",
        strip.text = element_text(face = "bold", size = 18,  hjust = 0, vjust = 0, margin = margin(0, 0, 10, 0))) +
  guides(color = guide_legend(title.position = "bottom", ncol = 2, order = 1, override.aes = list(shape = 21)),
         fill = guide_legend(title.position = "bottom", ncol = 2, order = 1),
         shape = guide_legend(title.position = "bottom", nrow = 2, order = 2, override.aes = list(fill = "white")))

saveGGplot(grob, figure_name, "output/figures", width = 10, height = 14)




# 4 Extended Data 1-6 -----------------------------------------------------

applyEnv()

figure_name <- "ED_FigNUMBER_REGION-vs-frontier.pdf"

sarahLoad(c("cause_hierarchy", "frontier_scaled", "region"), folder = "data/processed")

temp1 <- region %>%
  filter(region != "Latin America & Caribbean", year >= 2000, year <= 2050, age >= 20, causename %in% focus) %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females"),
         age2 = makeDisplayAgeGroup(age),
         age = makeAgeGroup(age),
         causename = factor(causename, levels = focus)) %>%
  group_by(region, year, sex, age2, ghecause, causename) %>%
  summarize_at(vars(dths, pop), ~sum(.), .groups = "drop") %>%
  mutate(dths_rate = dths / pop * 100000) %>%
  ungroup() %>%
  select(region, year, sex.frontier = sex, age2, causename, dths_rate)

temp2 <- frontier_scaled %>%
  filter(year >= 2000, year <= 2050, age >= 20, causename %in% focus) %>%
  mutate(sex.frontier = "Frontier",
         age2 = makeDisplayAgeGroup(age),
         age = makeAgeGroup(age),
         causename = factor(causename, levels = focus)) %>%
  group_by(year, sex, age2, ghecause, causename) %>%
  mutate(region = "Frontier", frontier = mean(frontier)) %>%
  ungroup() %>%
  select(region, year, sex.frontier, age2, causename, dths_rate = frontier)

ggdata <- bind_rows(temp1, temp2) %>%
  mutate(sex.frontier = factor(sex.frontier, levels = c("Females", "Males", "Frontier")),
         dths_rate = ifelse(year %in% covid, NA, dths_rate)) %>%
  arrange(region, year, age2, causename, rev(sex.frontier))

fills <- setNames(colorFunct(length(levels(ggdata$sex.frontier))), levels(ggdata$sex.frontier))
colors <- setNames(darken(fills, 0.3), levels(ggdata$sex.frontier))

figs <- list()
for(j in unique(ggdata$region[ggdata$region != "Frontier"])){

  panels <- list()
  for(i in unique(ggdata$causename)){

    ggdata2 <- ggdata %>% filter(region %in% c(j, "Frontier"), causename == i)

    ylims <- ggRange(ggdata2$dths_rate)
    if(ylims[1] <= 0){
      ylims <- c(0.1, ylims[2])
    }
    ylab <- ifelse(i == "Cardiovascular diseases", "Mortality rate (per 100K, log scale)", "")

    panels[[i]] <- ggplot(ggdata2) +
      facet_wrap(~ age2, nrow = 1) +
      geom_line(aes(x = year, y = dths_rate, color = sex.frontier, group = sex.frontier), alpha = 0.3) +
      geom_line(data = subset(ggdata2, year >= 2020),
                aes(x = year, y = dths_rate, color = sex.frontier, group = sex.frontier)) +
      geom_point(data = subset(ggdata2, year < 2020),
                 aes(x = year, y = dths_rate, color = sex.frontier, fill = sex.frontier),
                 shape = 21, size = 1) +
      labs(title = i) +
      scale_x_continuous("", breaks = seq(2000, 2050, 10), labels = c("'00", "'10", "'20", "'30", "'40", "'50"),
                         expand = expansion(mult = 0.1)) +
      scale_y_continuous(ylab, trans = "log",
                         limits = ylims, breaks = log.breaks, labels = log.labels) +
      scale_color_manual("", values = colors) +
      scale_fill_manual("", values = fills) +
      theme(axis.title.x = element_blank(),
            legend.position = "bottom")

    if(i != tail(unique(ggdata$causename), 1)){
      panels[[paste(i, "spacer")]] <- ""
    }

  }

  figs[[j]] <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.025, 1, -0.025, 1, -0.025, 1, -0.025, 1),
                         common.legend = TRUE, legend = "bottom")
  number <- which(unique(ggdata$region[ggdata$region != "Frontier"]) == j)
  figure_name_ <- gsub("NUMBER", number, gsub("REGION", slug(j), figure_name))
  saveGGplot(figs[[j]], figure_name_, "output/figures", width = 9, height = 12)


}

