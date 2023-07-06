
### 5.4 Appendix figures



# 1 Fig A1 Frontier comparison --------------------------------------------

applyEnv()

figure_name <- "FigA1_INFO.pdf"

sarahLoad("frontier_base", folder = "data/processed")

ggdata <- frontier_base %>%
  left_join(cause_hierarchy %>% select(ghecause, parent_ghecause, parent_causename),
            by = "ghecause") %>%
  filter(age >= 20, is.na(sex) | sex == 2, definition %in% c("Minimum", "10th percentile")) %>%
  mutate(causename = ifelse(causename %in% sex.specific, paste(causename, "(females)"), causename),
         parent_causename = ifelse(parent_causename %in% sex.specific, paste(parent_causename, "(females)"), parent_causename)) %>%
  mutate(display.causename = factor(wrapper(causename, 35), levels = wrapper(causename.levels2, 35)),
         frontier = ifelse(frontier < 0.1, 0, frontier)) %>%
  mutate(group = paste(definition, year))

colors <- colorFunct(2, names = c("10th percentile", "Minimum"), color.and.fill = TRUE)

for(parent in unique(ggdata$parent_causename)[!is.na(unique(ggdata$parent_causename))]){

  ggdata2 <- ggdata %>% filter(causename == parent | parent_causename == parent)
  gglist <- list("parent" =  ggdata %>% filter(causename == parent),
                 "children" = ggdata %>% filter(parent_causename == parent))

  id <- ids[which(unique(ggdata$parent_causename)[!is.na(unique(ggdata$parent_causename))] == parent)]

  ylims <- ggRange(ggdata2$frontier)
  if(all(ylims != 0)){ylims.log <- ylims}
  if(ylims[1] == 0){ylims.log <- c(0.1, ylims[2])}
  if(all(ylims == 0)){ylims.log <- c(0.1, 5)}

  panels <- list()
  for(i in names(gglist)){

    xlab <- ifelse(i == "parent", "Age", "")
    ylab <- ifelse(i == "parent", "Frontier mortality rate (per 100K, log scale)", "")

    base <- ggplot(gglist[[i]]) +
      # Minimum
      geom_line(data = subset(gglist[[i]], definition == "Minimum"),
                aes(x = age, y = frontier, group = group, color = year)) +
      scale_color_material(palette = "amber", breaks = c(2000, 2005, 2010, 2015, 2019),
                           guide = guide_colorbar(title = "Minimum", title.position = "top",
                                                  frame.colour = "black", ticks = TRUE, ticks.colour = "black",
                                                  nbin = 10, label.position = "bottom", label.hjust = 0.5,
                                                  barwidth = 10, barheight = 1,
                                                  direction = 'horizontal', order = 1)) +
      new_scale_color() +
      # 10th percentile
      geom_line(data = subset(gglist[[i]], definition == "10th percentile"),
                aes(x = age, y = frontier, group = group, color = year)) +
      scale_color_material(palette = "red", breaks = c(2000, 2005, 2010, 2015, 2019),
                           guide = guide_colorbar(title = "10th percentile", title.position = "top",
                                                  frame.colour = "black", ticks = TRUE, ticks.colour = "black",
                                                  nbin = 10, label.position = "bottom", label.hjust = 0.5,
                                                  barwidth = 10, barheight = 1,
                                                  direction = 'horizontal', order = 2)) +
      scale_x_continuous(xlab) +
      scale_y_continuous(ylab, trans = "log",
                         limits = ylims.log, breaks = log.breaks, labels = log.labels) +
      theme(legend.position = "bottom")

    if(i == "parent"){

      panels[[paste(id, i)]] <- base +
        labs(title = wrapper(paste(id, parent), 60))

      saveGGplot(x = panels[[paste(id, i)]],
                 name = gsub("INFO", slug(paste0(gsub("[()]", "", id), 1)), figure_name),
                 folder = "output/figures/appendix/FigA1 panels", width = 7.5, height = 5)

    }else{

      panels[[paste(id, i)]] <- base +
        labs(subtitle = "Lower level causes") +
        facet_wrap(~ display.causename, ncol = 2) +
        theme(legend.position = "none")

      h <- ceiling(length(unique(gglist[["children"]]$ghecause)) / 2) * 2.5
      saveGGplot(x = panels[[paste(id, i)]],
                 name = gsub("INFO", slug(paste0(gsub("[()]", "", id), 2)), figure_name),
                 folder = "output/figures/appendix/FigA1 panels", width = 7.5, height = h)

    }

  }

}



# 2 Fig A2 Frontier process -----------------------------------------------

applyEnv()

figure_name <- "FigA2_frontier-process.pdf"

sarahLoad(c("cause_hierarchy", "frontier_info/frontier_info_4"),
          folder = "data/processed")

temp1 <- frontier_info_4 %>%
  filter(age >= 30, year >= 2000, year <= 2050, definition == "10th percentile") %>%
  mutate(projected = ifelse(is.na(harmonized), projected, harmonized)) %>%
  mutate(causename = case_when(sex == 1 ~ paste0(causename, " (males)"),
                               sex == 2 ~ paste0(causename, " (females)"),
                               TRUE ~ causename)) %>%
  pivot_longer(cols = c(base, harmonized, projected, scaled), names_to = "stage", values_to = "frontier") %>%
  mutate(stage = capitalize(stage)) %>%
  mutate(stage = factor(stage, levels = c("Base", "Harmonized", "Projected", "Scaled"))) %>%
  mutate(observed = year < 2020) %>%
  filter(!is.na(frontier))

ggdata <- temp1 %>%
  filter(is.na(sex) | sex == 2) %>%
  mutate(age2 = makeMathersAgeGroup(age),
         age = makeAgeGroup(age),
         frontier = ifelse(frontier < 0.1, 0, frontier)) %>%
  mutate(zero = frontier == 0) %>%
  arrange(age)

color.info <- groupColorInfo(ggdata, "age2", "age")
colors <- groupColor(color.info$n, color.info$names, color.and.fill = TRUE)
shapes <- c(21, NA)

grobs <- list()
for(i in unique(ggdata$causename)){

  ggdata2 <- ggdata %>% filter(causename == i)
  id <- ids[which(unique(ggdata$causename) == i)]

  ylims <- ggRange(ggdata2$frontier)
  if(all(ylims == 0)){
    ylims <- c(0.1, 1)
  }
  if(ylims[1] == 0){
    ylims <- c(0.1, ylims[2])
  }

  figure <- ggplot(ggdata2) +
    facet_grid(cols = vars(stage), rows = vars(age2)) +
    geom_line(data = subset(ggdata2, !observed),
              aes(x = year, y = frontier, color = age)) +
    geom_point(data = subset(ggdata2, observed),
               aes(x = year, y = frontier, color = age, fill = age, shape = zero)) +
    labs(title = paste(id, i)) +
    scale_x_continuous("", breaks = seq(2000, 2050, 10), labels = c("'00", "'10", "'20", "'30", "'40", "'50")) +
    scale_y_continuous("Frontier mortality rate (per 100K, log scale)", trans = "log",
                       limits = ylims, breaks = log.breaks, labels = log.labels) +
    scale_color_manual("", values = colors$colors) +
    scale_fill_manual("", values = colors$fills) +
    scale_shape_manual("", values = shapes) +
    theme(axis.line = element_blank(), axis.title.x = element_blank(),
          legend.position = "bottom",
          panel.border = element_rect(color = alpha("black", 0.4), size = 0.2)) +
    guides(fill = guide_legend(nrow = 2),
           color = guide_legend(nrow = 2, override.aes = list(pch = 21)),
           shape = "none")

  grobs[[id]] <- as_grob(figure)

}

saveGGplot(grobs, figure_name, folder = "output/figures/appendix", width = 12, height = 10, multipage = TRUE)



# 3 Fig B1 Frontier -------------------------------------------------------

applyEnv()

sarahLoad("frontier_scaled", folder = "data/processed")

figure_name <- "FigB1_frontier.pdf"

ggdata <- frontier_scaled %>%
  filter(year >= 2000, year <= 2050, is.na(sex) | sex == 2, age >= 30, definition == "10th percentile") %>%
  left_join(cause_hierarchy %>% select(ghecause, prefix), by = "ghecause") %>%
  mutate(causename = ifelse(!is.na(sex), paste(causename, "(females)"), causename),
         frontier = ifelse(frontier < 0.01, 0, frontier),
         age2 = makeMathersAgeGroup(age),
         age = makeAgeGroup(age),
         observed = year < 2020) %>%
  mutate(causename = factor(causename, levels = causename.levels2),
         zero = frontier == 0) %>%
  arrange(prefix, age)

color_info <- groupColorInfo(ggdata, "age2", "age")
colors <- groupColor(color_info$n, color_info$names, color.and.fill = TRUE)

grobs <- list()
for(i in unique(ggdata$causename)){

  ggdata2 <- ggdata %>% filter(causename == i)

  id <- ids[which(unique(ggdata$causename) == i)]

  panels <- list()

  ylims <- ggRange(ggdata2$frontier)
  if(all(ylims != 0)){ylims.log <- ylims}
  if(ylims[1] == 0){ylims.log <- c(0.1, ylims[2])}
  if(all(ylims == 0)){
    ylims <- c(0, 5)
    ylims.log <- c(0.1, 5)
  }

  base <- ggplot(ggdata2) +
    facet_wrap(~ age2, nrow = 1) +
    geom_line(data = subset(ggdata2, observed),
              aes(x = year, y = frontier, color = age), alpha = 0.3) +
    geom_point(data = subset(ggdata2, observed),
               aes(x = year, y = frontier, fill = age, color = age), pch = 21) +
    geom_line(data = subset(ggdata2, !observed),
              aes(x = year, y = frontier, color = age)) +
    scale_x_continuous("", breaks = seq(2000, 2050, 10), labels = c("'00", "'10", "'20", "'30", "'40", "'50"),
                       expand = expansion(mult = 0.1)) +
    scale_color_manual("", values = colors$colors) +
    scale_fill_manual("", values = colors$fills) +
    theme(axis.title.x = element_blank(),
          legend.position = "right",
          panel.background = element_rect(fill = NA)) +
    guides(color = guide_legend(ncol = 1), fill = guide_legend(ncol = 1))

  panels[["standard"]] <- base +
    labs(title = paste(id, i), subtitle = "Standard scale") +
    scale_y_continuous("Frontier mortality rate (per 100K)", limits = ylims,
                       labels = label_number(scale_cut = cut_short_scale()))

  panels[["spacer"]] <- ""

  panels[["log"]] <- base +
    labs(title = "", subtitle = "Log scale") +
    scale_y_continuous("Frontier mortality rate (per 100K, log scale)", trans = "log", limits = ylims.log,
                       breaks = log.breaks, labels = log.labels)

  grobs[[id]] <- ggarrange(plotlist = panels, ncol = 1, heights = c(1, -0.1 , 1),
                    align = "hv", common.legend = TRUE, legend = "right")

  if(i == tail(unique(ggdata$causename), 1)){
    saveGGplot(grobs, figure_name, folder = "output/figures/appendix", width = 11, height = 8.5, multipage = TRUE)
  }

}



# 5 Fig B2-B7 Regional mortality projections ------------------------------

applyEnv()

figure_name <- "FigB-ID.pdf"

sarahLoad("region", folder = "data/processed")

ggdata <- region %>%
  filter(age >= 30) %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females"),
         age2 = makeMathersAgeGroup(age),
         age = makeAgeGroup(age),
         dths_rate = ifelse(dths_rate < 0.1, 0, dths_rate),
         observed = year < 2020) %>%
  mutate(dths_rate = ifelse(year %in% covid, NA, dths_rate)) %>%
  mutate(sex = factor(sex, levels = c("Females", "Males")))

color.info <- groupColorInfo(ggdata, "age2", "age")
colors <- groupColor(color.info$n, color.info$names, color.and.fill = TRUE)

for(j in unique(ggdata$region)){

  grobs <- list()
  for(i in unique(ggdata$causename)){

    ggdata2 <- ggdata %>% filter(region == j, causename == i) %>% arrange(sex)
    id <- ids[which(unique(ggdata$causename) == i)]

    ylims <- ggRange(ggdata2$dths_rate)
    if(all(ylims != 0)){ylims.log <- ylims}
    if(ylims[1] == 0){ylims.log <- c(0.1, ylims[2])}
    if(all(ylims == 0)){
      ylims <- c(0, 5)
      ylims.log <- c(0.1, 5)
    }

    panels <- list()
    for(k in unique(ggdata2$sex)){

      ggdata3 <- ggdata2 %>% filter(sex == k)

      title <- ifelse(k == "Females", paste(id, i), " ")

      base <- ggplot(ggdata3) +
        facet_wrap(~age2, nrow = 1) +
        geom_line(data = subset(ggdata3, observed),
                  aes(x = year, y = dths_rate, color = age), alpha = 0.3) +
        geom_line(data = subset(ggdata3, !observed),
                  aes(x = year, y = dths_rate, color = age)) +
        geom_point(data = subset(ggdata3, observed),
                   aes(x = year, y = dths_rate, color = age, fill = age), pch = 21) +
        scale_x_continuous("", breaks = seq(2000, 2050, 20), labels = c("'00", "'20", "'40"),
                           expand = expansion(mult = 0.14)) +
        scale_color_manual("", values = colors$colors) +
        scale_fill_manual("", values = colors$fills) +
        theme(axis.title.x = element_blank(),
              legend.position = "bottom",
              panel.grid.minor.y = element_blank()) +
        guides(color = guide_legend(nrow = 2),
               fill = guide_legend(nrow = 2))

      panels[[paste(k, "standard")]] <- base +
        labs(title = title, subtitle = paste0(k, "   |   Standard scale")) +
        scale_y_continuous("Mortality rate (per 100K)",
                           limits = ylims, labels = label_number(scale_cut = cut_long_scale()))

      panels[[paste(k, "space")]] <- ""

      panels[[paste(k, "log")]] <- base +
        labs(subtitle = paste0(k, "   |   Log scale")) +
        scale_y_continuous("Mortality rate (per 100K, log scale)", trans = "log",
                           limits = ylims.log, breaks = log.breaks, labels = log.labels)

    }

    panels <- panels[c("Females standard", "Males standard",
                       "Females space", "Males space",
                       "Females log", "Males log")]

    grobs[[id]] <- ggarrange(plotlist = panels, nrow = 3, ncol = 2, align = "hv", heights = c(1, -0.1, 1),
                             common.legend = TRUE, legend = "bottom")

  }

    num <- which(unique(ggdata$region) == j) + 1; lab <- slug(j)
    figure_name2 <- gsub("-ID", paste(num, lab, sep = "_"), figure_name)
    saveGGplot(grobs, figure_name2, folder = "output/figures/appendix", width = 15, height = 10, multipage = TRUE)

}



# 6 Fig B8-B10 Economic value ---------------------------------------------

applyEnv()

figure_name <- "FigB-ID.pdf"

sarahLoad(c("cause_hierarchy", "cause_aesth"), folder = "data/processed")
sarahLoad("region_calculations", folder = "output/data")

ggdata <- region_calculations %>%
  filter(mece.lvl != 2, sex != 3) %>%
  left_join(cause_hierarchy %>% select(ghecause, main_causename), by = "ghecause") %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females")) %>%
  mutate(sex = factor(sex, levels = c("Females", "Males"))) %>%
  ungroup() %>%
  arrange(region, year, sex, mece.lvl, ghecause)

grobs <- list()
for(i in unique(ggdata$mece.lvl)){

  panels <- list()
  for(j in unique(ggdata$sex)){

    wrap <- 35
    ggdata2 <- ggdata %>% filter(mece.lvl == i, sex == j) %>%
      left_join(cause_aesth, by = c("ghecause", "causename", "mece.lvl")) %>%
      mutate(causename = wrapper(causename, wrap)) %>%
      mutate(causename = factor(causename, levels = wrapper(causename.levels, wrap)))

    aesth <- ggdata2 %>% select(causename, color) %>% unique()
    fills <- setNames(aesth$color, aesth$causename)
    colors <- setNames(darken(aesth$color, 0.3), aesth$causename)

    breaks <- wrapper(causename.levels, 35)[wrapper(causename.levels, 35) %in% unique(ggdata2$causename)]

    panels[[j]] <- ggplot(ggdata2) +
      facet_wrap(~ wrapper(region, 15), nrow = 1) +
      geom_col(aes(x = as.factor(year), y = v.r, fill = causename, color = causename)) +
      labs(title = j) +
      scale_x_discrete("") +
      scale_y_continuous("Proportion of annual income", limits = c(0, 0.5), expand = expansion(mult = c(0, 0.03))) +
      scale_color_manual(paste("Level", i, "causes"), values = colors, breaks = breaks) +
      scale_fill_manual(paste("Level", i, "causes"), values = fills, breaks = breaks) +
      theme(axis.line = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(),
            legend.position = "right",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_line(color = alpha("black", 0.2), size = 0.2)) +
      guides(color = guide_legend(ncol = 1, title.position = "top"))

    if(j == "Females"){
      panels[["spacer"]] <- ""
    }

  }

  grobs[[i]] <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.01, 1),
                          common.legend = TRUE, legend = "right")

  num <- which(unique(ggdata$mece.lvl) == i) + 8; lab <- slug(paste("economic value lvl", i))
  figure_name2 <- gsub("-ID", paste(num, lab, sep = "_"), figure_name)
  saveGGplot(grobs[[i]], figure_name2, "output/figures/appendix", width = 13, height = 12)

}



# 7 Fig B10-13 Economic value distribution --------------------------------

applyEnv()

figure_name <- "FigB-ID.pdf"

sarahLoad(c("cause_hierarchy", "cause_aesth"), folder = "data/processed")
sarahLoad("region_calculations", folder = "output/data")

ggdata <- region_calculations %>%
  filter(sex != 3) %>%
  left_join(cause_hierarchy %>% select(ghecause, main_causename), by = "ghecause") %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females")) %>%
  mutate(sex = factor(sex, levels = c("Females", "Males"))) %>%
  ungroup() %>%
  arrange(region, year, sex, mece.lvl, ghecause)

grobs <- list()
for(i in unique(ggdata$mece.lvl)){

  panels <- list()
  for(j in unique(ggdata$sex)){

    wrap <- 35
    ggdata2 <- ggdata %>% filter(mece.lvl == i, sex == j) %>%
      left_join(cause_aesth, by = c("ghecause", "causename", "mece.lvl")) %>%
      mutate(causename = wrapper(causename, wrap)) %>%
      mutate(causename = factor(causename, levels = wrapper(causename.levels, wrap)))

    aesth <- ggdata2 %>% select(causename, color) %>% unique()
    fills <- setNames(aesth$color, aesth$causename)
    colors <- setNames(darken(aesth$color, 0.3), aesth$causename)

    breaks <- wrapper(causename.levels, 35)[wrapper(causename.levels, 35) %in% unique(ggdata2$causename)]

    panels[[j]] <- ggplot(ggdata2) +
      facet_wrap(~ wrapper(region, 15), nrow = 1) +
      geom_col(aes(x = as.factor(year), y = v.r, fill = causename, color = causename), position = "fill") +
      labs(title = j) +
      scale_x_discrete("") +
      scale_y_continuous("Fraction", expand = expansion(mult = c(0, 0.03))) +
      scale_color_manual(paste("Level", i, "causes"), values = colors, breaks = breaks) +
      scale_fill_manual(paste("Level", i, "causes"), values = fills, breaks = breaks) +
      theme(axis.line = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(),
            legend.position = "right",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_line(color = alpha("black", 0.2), size = 0.2)) +
      guides(color = guide_legend(ncol = 1, title.position = "top"))

    if(j == "Females"){
      panels[["spacer"]] <- ""
    }

  }

  grobs[[i]] <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.01, 1),
                          common.legend = TRUE, legend = "right")

  num <- which(unique(ggdata$mece.lvl) == i) + 10; lab <- slug(paste("economic value dist lvl", i))
  figure_name2 <- gsub("-ID", paste(num, lab, sep = "_"), figure_name)
  saveGGplot(grobs[[i]], figure_name2, "output/figures/appendix", width = 13, height = 12)

}



# 8 Fig C1-C5 Sensitivity analyses ----------------------------------------

applyEnv()

# Figure name
figure_name <- "FigC-ID.pdf"

# Loading data
sarahLoad(c("cause_hierarchy", "cause_aesth"), folder = "data/processed")
sarahLoad("SA_region_calculations", folder = "output/data")

ggdata <- SA_region_calculations %>%
  filter(mece.lvl == 2, sex != 3) %>%
  left_join(cause_hierarchy %>% select(ghecause, main_causename), by = "ghecause") %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females")) %>%
  mutate(sex = factor(sex, levels = c("Females", "Males")),
         scenario = factor(scenario, levels = c("Minimum frontier definition",
                                                "Lower income elasticity", "Higher income elasticity",
                                                "OECD base income",
                                                "Lower discount rate", "Higher discount rate"))) %>%
  ungroup() %>%
  arrange(scenario, region, year, sex, mece.lvl, ghecause)

ylims <- c(0, ggRange(ggdata$v.r, accuracy = 0.1)[2])

panels <- list()
for(i in unique(ggdata$scenario)){

  id <- which(unique(ggdata$scenario) == i)
  id <- paste(id, slug(i), sep = "_")

  for(j in unique(ggdata$sex)){

    ggdata2 <- ggdata %>% filter(scenario == i, sex == j) %>%
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
      scale_y_continuous("Proportion of annual income", limits = ylims, expand = expansion(mult = c(0, 0.03))) +
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
      saveGGplot(grob, gsub("-ID", id, figure_name), "output/figures/appendix",  width = 13, height = 12)
    }

  }
}


# 9 Fig D1 Rankings -------------------------------------------------------

applyEnv()

figure_name <- "FigD1_rankings.pdf"

sarahLoad("region_calculations", folder = "output/data")
sarahLoad(c("cause_aesth", "ghe_recoded", "region"), folder = "data/processed")

temp1 <- region_calculations %>%
  filter(mece.lvl == 2, sex != 3) %>%
  group_by(region, year, sex) %>%
  mutate(rank = rank(-v.r)) %>%
  arrange(region, sex, year, desc(v.r)) %>%
  select(region, year, sex, causename, v.r_rank = rank)

temp2 <- region %>%
  filter(year %in% c(2000, 2019, 2050)) %>%
  group_by(region, year, sex, ghecause, causename) %>%
  summarize_at(vars(dths, pop), ~ sum(.)) %>%
  mutate(dths_rate = dths / pop * 100000) %>%
  left_join(cause_hierarchy %>% select(ghecause, mece_lvl2), by = "ghecause") %>%
  filter(mece_lvl2) %>%
  group_by(region, year, sex) %>%
  mutate(rank = rank(-dths_rate)) %>%
  arrange(region, sex, year, desc(dths_rate)) %>%
  select(region, year, sex, causename, dths.rate_rank = rank)

temp3 <- full_join(temp1, temp2, by = c("region", "year", "sex", "causename")) %>%
  left_join(cause_aesth %>% filter(mece.lvl == 2), by = "causename") %>%
  arrange(region, year, sex, v.r_rank, dths.rate_rank)

ggdata <- temp3 %>%
  filter(year != 2000) %>%
  pivot_longer(cols = ends_with("rank"), names_to = "rank.sys", values_to = "rank") %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females"),
         rank.sys = ifelse(rank.sys == "dths.rate_rank", "Mortality rate\nranking", "Economic value\nranking")) %>%
  mutate(rank.sys = factor(rank.sys, levels = c("Mortality rate\nranking", "Economic value\nranking")))

aesth <- ungroup(ggdata) %>% select(causename, color) %>% unique()
fills <- setNames(aesth$color, aesth$causename)
colors <- setNames(darken(aesth$color, 0.4), aesth$causename)
colors2 <- setNames(rep("black", length(aesth$causename)), aesth$causename)
colors2["Intentional injuries"] <- colors2["Unintentional injuries"] <- "white"
colors3 <- setNames(darken(fills, 0.1), aesth$causename)
colors3["Infectious and parasitic diseases"] <- darken(colors3["Infectious and parasitic diseases"], 0.3)
colors3["Maternal and neonatal conditions"] <- darken(colors3["Maternal and neonatal conditions"], 0.1)

grobs <- list()
for(i in unique(ggdata$region)){

  id <- ids[which(unique(ggdata$region) == i)]

  ggdata2 <- ggdata %>% filter(region == i)

  grobs[[i]] <- ggplot(ggdata2) +
    facet_grid(rows = vars(year), cols = vars(sex), scales = "free_x") +
    # Ranking label
    geom_text(data = subset(ggdata2, rank == 1),
              aes(x = rank.sys, y = 0, label = rank.sys),
              family = "Barlow", nudge_y = 0.5) +
    # Transparent thick line
    geom_line(aes(x = rank.sys, y = rank, color = causename, group = causename), size = 3, alpha = 0.5) +
    scale_color_manual(values = fills) +
    new_scale_color() +
    # Opaque thin line and circles
    geom_line(aes(x = rank.sys, y = rank, color = causename, group = causename)) +
    geom_point(aes(x = rank.sys, y = rank, color = causename, fill = causename), pch = 21, size = 9) +
    scale_color_manual(values = colors) +
    new_scale_color() +
    geom_text(aes(x = rank.sys, y = rank, label = rank, color = causename),
              family = "Barlow", fontface = "bold", size = 4) +
    scale_color_manual(values = colors2) +
    new_scale_color() +
    # Cause labels
    geom_text(data = subset(ggdata2, rank.sys == "Economic value\nranking"),
              aes(x = rank.sys, y = rank, label = causename, color = causename),
              family = "Barlow", hjust = 0, nudge_x = 0.15, size = 4) +
    scale_color_manual(values = colors3) +
    labs(title = paste(id, i)) +
    scale_x_discrete("", expand = expansion(mult = c(0.4, 1.85))) +
    scale_y_reverse("", expand = expansion(mult = c(0.06, 0.08))) +
    scale_fill_manual(values = fills) +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
          panel.background = element_rect(fill = "white", color = alpha("black", 0.2)), panel.grid.major = element_blank(),
          legend.position = "none")

  if(i == tail(unique(ggdata$region), 1)){
    saveGGplot(grobs, figure_name, "output/figures/appendix", width = 10, height = 10, multipage = TRUE)
  }

}



# 11 END ------------------------------------------------------------------

# Tidying environment and notifying the end of the script
notifyScript(); tidy()
# -------------------------------------------------------------------------

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



# Region comparison -------------------------------------------------------



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








# # 3 Fig 2 Latin America mortality trajectory ------------------------------
#
# sarahLoad("region", folder = "data/processed")
#
# figure_name <- "Fig2.pdf"
#
# ggdata <- region %>%
#   filter(grepl("Latin America", region),
#          causename %in% c("Cardiovascular diseases", "Malignant neoplasms",
#                           "Injuries", "All causes",
#                           "Communicable, maternal, perinatal and nutritional conditions"),
#          age >= 30) %>%
#   left_join(cause_hierarchy %>% select(ghecause, prefix), by = "ghecause") %>%
#   mutate(dths_rate = ifelse(dths_rate < 0.01, 0, dths_rate),
#          sex = case_when(sex == 1 ~ "Males",
#                          sex == 2 ~ "Females",
#                         TRUE ~ NA_character_),
#          age2 = makeDisplayAgeGroup(age),
#          age = makeAgeGroup(age)) %>%
#   mutate(zero = dths_rate == 0) %>%
#   arrange(prefix, region, age, sex, year) %>%
#   ungroup()
#
# color_info <- groupColorInfo(ggdata, "age2", "age")
# colors <- groupColor(color_info$n, color_info$names, color.and.fill = TRUE)
# shapes <- c(21, 0)
#
# grobs <- list()
# for(i in unique(ggdata$causename)){
#
#   ggdata2 <- ggdata %>% filter(causename == i)
#
#   id <- ids[which(unique(ggdata$causename) == i)]
#
#   ylims <- ggRange(ggdata2$dths_rate)
#   if(ylims[1] <= 0){
#     ylims <- c(0.1, ylims[2])
#   }
#
#   Panels <- list()
#   for(j in unique(ggdata2$sex)){
#
#     ggdata3 <- ggdata2 %>% filter(sex == j)
#     panels <- list()
#
#     title <- ifelse(j == "Females", paste(id, i), "")
#
#     panels[["Standard"]] <- ggplot(ggdata3) +
#       facet_wrap(~ age2, nrow = 1) +
#       geom_line(data = subset(ggdata3, year >= 2020), aes(x = year, y = dths_rate, color = age)) +
#       geom_point(data = subset(ggdata3, year < 2020), aes(x = year, y = dths_rate, fill = age, color = age, shape = zero)) +
#       labs(title = title,
#            subtitle = paste0(j, "   |   Standard scale")) +
#       scale_x_continuous("", breaks = seq(2000, 2040, 20), labels = c("2000", "'20", "'40")) +
#       scale_y_continuous("Mortality rate (per 100K)", limits = ylims, labels = label_number(scale_cut = cut_long_scale())) +
#       scale_color_manual("", values = colors$colors) +
#       scale_fill_manual("", values = colors$fills) +
#       scale_shape_manual("", values = shapes) +
#       theme(axis.title.x = element_blank(),
#             panel.background = element_rect(fill = NA),
#             legend.position = "bottom",
#             panel.grid.minor.x = element_line(color = alpha("black", 0.2), size = 0.2)) +
#       guides(color = guide_legend(nrow = 2), shape = "none")
#
#     if(j == "Males"){
#       panels[["Standard"]] <- panels[["Standard"]] + theme(axis.title.y = element_blank())
#     }
#
#     panels[[paste(j, "Spacer")]] <- ""
#
#     panels[["Log"]] <- ggplot(ggdata3) +
#       facet_wrap(~ age2, nrow = 1) +
#       geom_line(data = subset(ggdata3, year >= 2020), aes(x = year, y = dths_rate, color = age)) +
#       geom_point(data = subset(ggdata3, year < 2020), aes(x = year, y = dths_rate, fill = age, color = age, shape = zero)) +
#       labs(subtitle = paste0(j, "   |   Log scale")) +
#       scale_x_continuous("", breaks = seq(2000, 2040, 20), labels = c("2000", "'20", "'40")) +
#       scale_y_continuous("Mortality rate (per 100K)", trans = "log",
#                          limits = ylims, breaks = log.breaks, labels = log.labels) +
#       scale_color_manual("", values = colors$colors) +
#       scale_fill_manual("", values = colors$fills) +
#       scale_shape_manual("", values = shapes) +
#       theme(axis.title.x = element_blank(),
#             panel.background = element_rect(fill = NA),
#             panel.grid.minor.x = element_line(color = alpha("black", 0.2), size = 0.2))
#
#     if(j == "Males"){
#       panels[["Log"]] <- panels[["Log"]] + theme(axis.title.y = element_blank())
#     }
#
#     Panels[[j]] <- ggarrange(plotlist = panels, ncol = 1, heights = c(1, -0.1, 1),
#                              align = "hv", legend = "none")
#
#   }
#
#   figure <- ggarrange(plotlist = Panels, ncol = 2, align = "hv", widths = c(1.05, 1),
#                       common.legend = TRUE, legend = "bottom", legend.grob = get_legend(panels[[1]]))
#
#   grobs[[id]] <- cowplot::as_grob(figure)
#
# }
#
# saveGGplot(grobs, figure_name, "output/figures", width = 13, height = 8, multipage = TRUE)
#
#
#



# 5 Fig 4 economic values -------------------------------------------------

figure_name <- "Fig4-OPTION.pdf"

sarahLoad("region_calculations", folder = "data/processed")

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

# # 4 Fig 3 Proportion of of GNI -------------------------------------------
#
# # Figure name
# figure_name <- "Fig3.pdf"
#
# # Loading data
# sarahLoad("cause_hierarchy", folder = "data/processed")
# sarahLoad("region_calculations", folder = "output/data")
#
# wrap <- 35
#
# ggdata <- region_calculations %>%
#   left_join(cause_hierarchy %>% select(ghecause, main_causename)) %>%
#   mutate(sex = ifelse(sex == 1, "Males", "Females"),
#          causename = wrapper(causename, wrap),
#          main_causename = factor(main_causename, levels = c("Communicable, maternal, perinatal and nutritional conditions",
#                                                             "Noncommunicable diseases", "Injuries"))) %>%
#   arrange(region, sex, mece.lvl, main_causename, causename, year) %>%
#   ungroup()
#
# grobs <- list()
# for(i in unique(ggdata$mece.lvl)){
#
#   id <- ids[which(unique(ggdata$mece.lvl) == i)]
#
#   panels <- list()
#   for(j in unique(ggdata$sex)){
#
#     ggdata2 <- ggdata %>% filter(mece.lvl == i, sex == j) %>%
#       mutate(causename = factor(causename, levels = wrapper(causename.levels, wrap)))
#
#     if(j == "Females"){
#       title <- paste(id, "Level", i, "causes")
#     }else{
#       title <- ""
#     }
#
#     grey.base <- groupColor(n = 9, palettes = "grey")[c(4:2, 7:9)]
#     grey.info <- groupColorInfo(ggdata2 %>% filter(main_causename != "Noncommunicable diseases"),
#                                 group = "main_causename", subgroup = "causename")
#
#     grey.fills <- grey.base[c(1:grey.info$n[1], (6+1-grey.info$n[2]):6)]
#     grey.colors <- darken(grey.fills, 0.3)
#
#     greys <- list(colors = setNames(grey.colors, grey.info$names),
#                   fills = setNames(grey.fills, grey.info$names))
#
#     names <- ggdata2 %>% filter(main_causename == "Noncommunicable diseases") %>%
#       arrange(main_causename, causename) %>%
#       pull(causename) %>% unique()
#     names <- levels(names)[names]
#     colors <- colorFunct(n = length(names), names = names, color.and.fill = TRUE)
#
#     colors[["colors"]] <- c(colors[["colors"]], greys[["colors"]])
#     colors[["fills"]] <- c(colors[["fills"]], greys[["fills"]])
#
#     breaks <- wrapper(causename.levels, wrap)[wrapper(causename.levels, wrap) %in% unique(ggdata2$causename)]
#
#     panels[[j]] <- ggplot(ggdata2) +
#       facet_wrap(~ wrapper(region, 15), nrow = 1) +
#       geom_col(aes(x = as.factor(year), y = v.r, fill = causename, color = causename)) +
#       labs(title = title, subtitle = j) +
#       scale_x_discrete("") +
#       scale_y_continuous("Proportion of GNI per capita", limits = c(0, 0.5),
#                          expand = expansion(mult = c(0, 0.05))) +
#       scale_color_manual(paste("Level", i,"causes"), values = colors$colors,
#                          breaks = breaks) +
#       scale_fill_manual(paste("Level", i,"causes"), values = alpha(colors$fills, 0.9),
#                         breaks = breaks) +
#       theme(axis.line = element_blank(), axis.ticks.x = element_blank(),
#             legend.position = "right", panel.grid.major.x = element_blank()) +
#       guides(color = guide_legend(ncol = 1, title.position = "top"))
#
#     if(which(unique(ggdata$sex) == j) == 1){
#       panels[["spacer"]] <- ""
#     }
#
#   }
#
#   grobs[[id]] <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.1, 1),
#                            common.legend = TRUE, legend = "right")
#
# }
#
# saveGGplot(grobs, figure_name, "output/figures", width = 14, height = 15)

