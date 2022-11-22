
### 4.1 Country calculations



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("chang_envelope", "country_info", "country_scaled", "frontier_lagged", "population"),
          folder = "data/processed")
envelope <- read.csv("data/input/chang_envelope.csv", as.is = TRUE)


# 2 Mortality differential ------------------------------------------------

# Prepping country data
country_scaled %<>%
  filter(year %in% c(2000, 2019, 2040)) %>%
  mutate(sex_match = ifelse(causename %in% sex.specific, sex, NA)) %>%
  dplyr::select(iso3, year, age, sex_match, sex, ghecause, causename, dths_rate) %>%
  arrange(iso3, year, age, ghecause, sex)

# Prepping frontier data
frontier_lagged %<>%
  filter(year %in% c(2000, 2019, 2040)) %>%
  dplyr::select(year, age, sex_match = sex, ghecause, causename, frontier) %>%
  arrange(year, age, ghecause, sex_match)

# Prepping population data
population %<>%
  filter(year %in% c(2000, 2019, 2040))

# Combining country and frontier data
data <- inner_join(country_scaled, frontier_lagged,
                   by = c("year", "age", "sex_match", "ghecause", "causename")) %>%
  mutate_at(vars(dths_rate, frontier), ~ . /100000) %>%
  dplyr::select(-sex_match) %>%
  arrange(iso3, ghecause, age, sex, year)

# Checking data
containsNA(data)



# 3 Calculations ----------------------------------------------------------

# * delta -----------------------------------------------------------------
# Difference between frontier mortality and country mortality
delta <- data %>%
  mutate(delta = exp(-1 * frontier) - exp(-1 * dths_rate)) %>%
  mutate(delta = ifelse(delta < 0, 0, delta))

# Checking data
containsNA(delta)


# * p ---------------------------------------------------------------------
# Proportion of avoidable mortality by cause

temp1 <- delta %>%
  left_join(cause_hierarchy %>% select(ghecause, starts_with("mece")), by = "ghecause") %>%
  pivot_longer(cols = starts_with("mece"), names_to = "mece.lvl", values_to = "mece.ind") %>%
  mutate(mece.lvl = as.factor(as.numeric(substr(mece.lvl, nchar(mece.lvl), nchar(mece.lvl)))))

temp2 <- list()
for(i in unique(temp1$mece.lvl)){
  temp2[[i]] <- temp1 %>%
    filter(mece.lvl == i, mece.ind) %>%
    group_by(iso3, year, age, sex, mece.lvl) %>%
    mutate(Delta = sum(delta)) %>%
    ungroup() %>% select(-mece.ind)
  if(i == tail(unique(temp1$mece.lvl), 1)){
    temp2 <- bind_rows(temp2)
  }
}

p <- temp2 %>%
  mutate(p = delta / Delta) %>%
  select(iso3, year, age, sex, ghecause, causename, mece.lvl, everything()) %>%
  arrange(iso3, sex, age, ghecause, mece.lvl)

# Checking data
# - Verifying p sums to 1
check <- p %>%
  group_by(iso3, year, age, sex, mece.lvl) %>%
  summarize(p = sum(p), .groups = "drop")
check %>% filter(round(p, 4) != 1)
# - Checking NA, NAN, infinite
containsNA(p)

p <- p %>% mutate(p = ifelse(is.nan(p), 0, p))

containsNA(p)


# * alpha ------------------------------------------------------------------

# Age-sex weights
alpha <- population %>%
  group_by(iso3, year, sex) %>%
  mutate(alpha = pop / sum(pop)) %>%
  ungroup() %>%
  select(iso3, year, age, sex, alpha)

# Checking data
# = Verifying alpha sums to 1
check <- alpha %>%
  group_by(iso3, year, sex) %>%
  summarize(alpha = sum(alpha), .groups = "drop")
check %>% filter(round(alpha, 4) != 1)
# - Checking NA, NAN, infinite
containsNA(alpha)
# - Checking consistency w envelope
check <- alpha %>%
  left_join(envelope %>% select("iso3", "year", "age", "sex", "pop.sex.weight"),
            by = c("iso3", "year", "age", "sex"))
check %>% filter(round(alpha, 4) != round(pop.sex.weight, 4))

alpha <- p %>% left_join(alpha, by = c("iso3", "year", "age", "sex"))


# * v.c --------------------------------------------------------------------

v.country <- inner_join(alpha, envelope %>% select("iso3", "year", "age", "sex", "b_log_1yr"),
                        by = c("iso3", "year", "age", "sex")) %>%
  mutate(v.c = alpha * p * b_log_1yr) %>%
  group_by(iso3, year, sex, ghecause, causename, mece.lvl) %>%
  summarize(v.c = sum(v.c), .groups = "drop") %>%
  arrange(iso3, year, sex, mece.lvl, ghecause)

country_calculations <- v.country

# __ + country_calculations -----------------------------------------------
sarahSave("country_calculations", folder = "output/data")
write.csv(country_calculations, file = "output/data/country_calculations.csv",
          na = "", row.names = FALSE)


# * w ---------------------------------------------------------------------
# Country weights

w <- population %>%
  filter(iso3 %in% unique(v.country$iso3)) %>%
  group_by(region, iso3, year, sex) %>%
  summarize(pop = sum(pop), .groups = "drop") %>%
  group_by(region, year, sex) %>%
  mutate(w = pop / sum(pop)) %>%
  ungroup() %>% select(-pop)

# Checking data
# = Verifying alpha sums to 1
check <- w %>%
  group_by(region, year, sex) %>%
  summarize(w = sum(w), .groups = "drop")
check %>% filter(round(w, 4) != 1)
# - Checking NA, NAN, infinite
containsNA(w)

w <- v.country %>% left_join(w, by = c("iso3", "year", "sex")) %>%
  select(region, iso3, everything())

containsNA(w)


# * v.r --------------------------------------------------------------------

v.region <- w %>%
  mutate(v.r = w * v.c) %>%
  group_by(region, year, sex, ghecause, causename, mece.lvl) %>%
  summarize(v.r = sum(v.r), .groups = "drop") %>%
  arrange(region, year, sex, mece.lvl, ghecause)

# Checking data
# = Verifying China and India v.c = v.r
check <- v.country %>% filter(iso3 %in% c("CHN", "IND")) %>%
  mutate(region = ifelse(iso3 == "CHN", "China", "India")) %>%
  left_join(v.region, by = c("year", "sex", "ghecause", "causename", "mece.lvl", "region"))
check %>% filter(v.c != v.r)
# - Checking NA, NAN, infinite
containsNA(v.region)


# * R_bar -----------------------------------------------------------------
R_bar <- v.region %>%
  group_by(region, year, sex, mece.lvl) %>%
  dplyr::mutate(R_bar = v.r / sum(v.r))

# Checking data
# = Verifying alpha sums to 1
check <- R_bar %>%
  group_by(region, year, sex, mece.lvl) %>%
  summarize(R_bar = sum(R_bar), .groups = "drop")
check %>% filter(round(R_bar, 4) != 1)
# - Checking NA, NAN, infinite
containsNA(R_bar)

region_calculations <- R_bar


# __ + region_calculations ------------------------------------------------
sarahSave("region_calculations", folder = "output/data")
write.csv(region_calculations, file = "output/data/region_calculations.csv",
          na = "", row.names = FALSE)

# 4 Graphing --------------------------------------------------------------

wrap <- 45

# _ A ---------------------------------------------------------------------

ggdata <- R_bar %>%
  left_join(cause_hierarchy %>% select(ghecause, main_causename), by = "ghecause") %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females"),
         main_causename = factor(main_causename,
                                 levels = c("Communicable, maternal, perinatal and nutritional conditions","Noncommunicable diseases", "Injuries")),
         causename = wrapper(causename, wrap)) %>%
  arrange(region, sex, mece.lvl, main_causename, causename, year) %>%
  ungroup()

grobs <- list()
for(i in unique(ggdata$mece.lvl)){

  id <- ids[which(unique(ggdata$mece.lvl) == i)]

  panels <- list()
  for(j in unique(ggdata$sex)){

    ggdata2 <- ggdata %>% filter(mece.lvl == i, sex == j) %>%
      mutate(causename = factor(causename, levels = wrapper(causename.levels, wrap)))

    if(j == "Females"){
      title <- paste(id, "Level", i, "causes")
    }else{
      title <- ""
    }

    ylims <- ggRange(ggdata %>% filter(mece.lvl == i) %>% pull(v.r), 0.05)

    grey.info <- groupColorInfo(ggdata2 %>%
                                  filter(main_causename != "Noncommunicable diseases"),
                                "main_causename", "causename")

    greys <- groupColor(n = 4, palettes = "grey", color.and.fill = TRUE, show = TRUE)
    greys$colors <- rev(greys$colors); greys$fills <- rev(greys$fills)
    greys$colors <- setNames(greys$colors[1:sum(grey.info$n)], grey.info$names)
    greys$fills <- setNames(greys$colors[1:sum(grey.info$n)], grey.info$names)

    colors <- ggdata2 %>% filter(main_causename == "Noncommunicable diseases") %>%
      arrange(main_causename, causename) %>%
      pull(causename) %>% unique()
    colors <- colorFunct(n = length(colors), names = colors, color.and.fill = TRUE)

    colors[["colors"]] <- c(colors[["colors"]], greys[["colors"]])
    colors[["fills"]] <- c(colors[["fills"]], greys[["fills"]])

    breaks <- wrapper(causename.levels, wrap)[wrapper(causename.levels, wrap) %in% unique(ggdata2$causename)]

    panels[[j]] <- ggplot(ggdata2) +
      facet_wrap(~ wrapper(region, 15), nrow = 1) +
      geom_col(aes(x = as.factor(year), y = v.r, fill = causename, color = causename)) +
      labs(title = title, subtitle = j) +
      scale_x_discrete("") +
      scale_y_continuous("Proportion of GNI per capita", limits = ylims,
                         expand = expansion(mult = c(0, 0.05))) +
      scale_color_manual(paste("Level", i,"causes"), values = colors$colors,
                         breaks = breaks) +
      scale_fill_manual(paste("Level", i,"causes"), values = alpha(colors$fills, 0.9),
                        breaks = breaks) +
      theme(axis.line = element_blank(), axis.ticks.x = element_blank(),
            legend.position = "right", panel.grid.major.x = element_blank()) +
      guides(color = guide_legend(ncol = 1, title.position = "top"))

    if(which(unique(ggdata$sex) == j) == 1){
      panels[["spacer"]] <- ""
    }

  }

  grobs[[id]] <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.1, 1),
                           common.legend = TRUE, legend = "right")

}

saveGGplot(grobs, "4_calculations_A.pdf", "output/figures", width = 14, height = 15)


# _ B ---------------------------------------------------------------------

grobs <- list()
for(i in unique(ggdata$mece.lvl)){

  id <- ids[which(unique(ggdata$mece.lvl) == i)]

  panels <- list()
  for(j in unique(ggdata$sex)){

    ggdata2 <- ggdata %>% filter(mece.lvl == i, sex == j) %>%
      mutate(causename = factor(causename, levels = wrapper(causename.levels, wrap)))

    if(j == "Females"){
      title <- paste(id, "Level", i, "causes")
    }else{
      title <- ""
    }

    ylims <- ggRange(ggdata %>% filter(mece.lvl == i) %>% pull(R_bar), 0.05)

    grey.info <- groupColorInfo(ggdata2 %>%
                                  filter(main_causename != "Noncommunicable diseases"),
                                "main_causename", "causename")

    greys <- groupColor(n = 4, palettes = "grey", color.and.fill = TRUE, show = TRUE)
    greys$colors <- rev(greys$colors); greys$fills <- rev(greys$fills)
    greys$colors <- setNames(greys$colors[1:sum(grey.info$n)], grey.info$names)
    greys$fills <- setNames(greys$colors[1:sum(grey.info$n)], grey.info$names)

    colors <- ggdata2 %>% filter(main_causename == "Noncommunicable diseases") %>%
      arrange(main_causename, causename) %>%
      pull(causename) %>% unique()
    colors <- colorFunct(n = length(colors), names = colors, color.and.fill = TRUE)

    colors[["colors"]] <- c(colors[["colors"]], greys[["colors"]])
    colors[["fills"]] <- c(colors[["fills"]], greys[["fills"]])

    breaks <- wrapper(causename.levels, wrap)[wrapper(causename.levels, wrap) %in% unique(ggdata2$causename)]

    panels[[j]] <- ggplot(ggdata2) +
      facet_wrap(~ wrapper(region, 15), nrow = 1) +
      geom_col(aes(x = as.factor(year), y = v.r, fill = causename, color = causename),
               position = position_fill()) +
      labs(title = title, subtitle = j) +
      scale_x_discrete("") +
      scale_y_continuous("Relative value of GNI per capita",
                         expand = expansion(mult = c(0, 0.05))) +
      scale_color_manual(paste("Level", i,"causes"), values = colors$colors,
                         breaks = breaks) +
      scale_fill_manual(paste("Level", i,"causes"), values = alpha(colors$fills, 0.9),
                        breaks = breaks) +
      theme(axis.line = element_blank(), axis.ticks.x = element_blank(),
            legend.position = "right", panel.grid.major.x = element_blank()) +
      guides(color = guide_legend(ncol = 1, title.position = "top"))

    if(which(unique(ggdata$sex) == j) == 1){
      panels[["spacer"]] <- ""
    }

  }

  grobs[[id]] <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.1, 1),
                           common.legend = TRUE, legend = "right")

}

saveGGplot(grobs, "4_calculations_B.pdf", "output/figures", width = 14, height = 15)


