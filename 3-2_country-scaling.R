
### 3.2 Country scaling

# This script takes the projected country mortality rates and scales them
# with the demographic longevity frontiers of the accompanying HLI paper,
# Chang et al. (2022), based on UN Population data.

# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("country_info", "country_projected",
            "country_projection_info/country_projection_info_1", "population"), folder = "data/processed")
envelope <- read.csv("data/input/chang_country.csv", as.is = TRUE) %>%
  filter(year >= 2000) %>%
  mutate(ghecause = 0, reference = mxn * 100000) %>%
  dplyr::select(iso3, year, sex, age, ghecause, reference)



# 2 Scaling with Chang et al. envelope ------------------------------------

# Setting the Chang et al.'s longevity frontier as the all-cause frontier
# and adding cause parents and levels
country_projected %<>%
  mutate(dths_rate = ifelse(is.na(dths_rate), projection, dths_rate)) %>%
  select(-projection)

data <- left_join(country_projected, envelope,
                  by = c("iso3", "year", "sex", "age", "ghecause")) %>%
  left_join(cause_hierarchy %>% select(ghecause, parent_ghecause, parent_causename, level),
            by = "ghecause") %>%
  mutate(dths_rate = ifelse(!is.na(reference), reference, dths_rate)) %>%
  select(-reference) %>%
  arrange(iso3, year, age, ghecause, sex)


# * 2.1 Level 0 -----------------------------------------------------------
# Reference level (Chang et al. longevity frontier)
scaled <- data %>%
  filter(level == 0)


# * 2.2 Level 1 -----------------------------------------------------------
# Scaling level 1 frontiers so they sum to the level 0 frontier
lvl1 <- scale(1, data, scaled)$scaled
scaled <- bind_rows(scaled, lvl1)


# * 2.3 Level 2 -----------------------------------------------------------
# Scaling level 2 frontiers so they sum to level 1 frontiers
lvl2 <- scale(2, data, scaled)$scaled
scaled <- bind_rows(scaled, lvl2)


# * 2.4 Level 3 -----------------------------------------------------------
# Scaling level 3 frontiers so they sum to level 2 frontiers
lvl3 <- scale(3, data, scaled)$scaled
scaled <- bind_rows(scaled, lvl3)


# * 2.5 Arranging data ----------------------------------------------------
country_scaled <- scaled %>%
  select(iso3, year, sex, age, ghecause, causename, dths_rate) %>%
  arrange(iso3, year, age, ghecause, sex)


# * 2.6 Checking scaling --------------------------------------------------

# Checking scaling
levels <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
concerns <- list()
for(i in levels){

  check <- country_scaled %>%
    left_join(cause_hierarchy %>% select(ghecause, mece = !!as.name(i)), by  = "ghecause") %>%
    filter(mece) %>%
    group_by(iso3, year, sex, age) %>%
    dplyr::summarize(lower_summed = sum(dths_rate), .groups = "drop") %>%
    left_join(envelope, by = c("iso3", "year", "sex", "age")) %>%
    mutate(sf = reference / lower_summed)

  concern <- check %>%
    filter(sf < 0.99 | sf > 1.01) %>%
    arrange(desc(sf))

  if(nrow(concern) > 0){
    warning(paste("Concerning scaling factors:", names(levels[levels == i])))
    concerns[[names(levels[levels == i])]] <- concern
  }
}

# __+ country_scaled -----------------------------------------------------
sarahSave("country_scaled", folder = "data/processed")


# * 2.7 Adding to country_projection_info dataframe -----------------------
country_projection_info_2 <- country_projection_info_1 %>%
  full_join(country_scaled %>% dplyr::rename(scaled = dths_rate),
            by = c("year", "iso3", "sex", "age", "ghecause", "causename")) %>%
  arrange(iso3, age, ghecause, year, sex)

# __+ country_projection_info_2 --------------------------------------------
sarahSave("country_projection_info_2", folder = "data/processed/country_projection_info")


exit()
# 3 Graphing --------------------------------------------------------------

iso3s <- population %>%
  filter(year == max(year), iso3 %notin% c("IND", "CHN")) %>%
  group_by(iso3) %>%
  summarize(pop = sum(pop)) %>%
  arrange(desc(pop)) %>%
  slice_head(n = 5) %>%
  pull(iso3)

ggdata <- country_projection_info_2 %>%
  filter(age >= 30, iso3 %in% iso3s) %>%
  mutate(age.ind = ifelse(age < 60, 1, 2)) %>%
  mutate(age = makeAgeGroup(age)) %>%
  pivot_longer(cols = c(base, projected, scaled), names_to = "stage", values_to = "dths_rate") %>%
  filter(!is.na(dths_rate)) %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females"),
         stage = capitalize(stage),
         dths_rate = ifelse(dths_rate < 0.1, 0, dths_rate)) %>%
  mutate(stage = factor(stage, levels = c("Base", "Projected", "Scaled")),
         zero = dths_rate == 0)

colors <- colorFunct(n = length(unique(ggdata$stage)), names = levels(ggdata$stage), color.and.fill = TRUE)

for(j in unique(ggdata$iso3)){

  country <- country_info$country[country_info$iso3 == j]
  ggdata2 <- ggdata %>% filter(iso3 == j)

  grobs <- list()
  for(i in unique(ggdata$causename)){

    id <- ids[which(unique(ggdata$causename) == i)]
    ggdata3 <- ggdata2 %>% filter(causename == i)

    panels <- list()
    for(k in unique(ggdata$sex)){

      ggdata4 <- ggdata3 %>% filter(sex  == k)
      ylims <- ggRange(ggdata4$dths_rate)
      if(ylims[1] <= 0){
        ylims <- c(0.1, ylims[2])
      }

      rows <- list()
      for(l in unique(ggdata$age.ind)){

        ggdata5 <- ggdata4 %>% filter(age.ind == l)
        ggdata_points <- ggdata5 %>% filter(year >= 2000, year < 2020, stage != "Projected")
        ggdata_lines <- ggdata5 %>% filter(year > 2019, year <= 2040)

        rows[[as.character(l)]] <- ggplot(ggdata5) +
          facet_wrap(~ age, nrow = 1) +
          geom_line(data = ggdata_lines, aes(x = year, y = dths_rate, color = stage), size = 0.75) +
          geom_point(data = ggdata_points,
                     aes(x = year, y = dths_rate, color = stage, fill = stage), pch = 21) +
          scale_x_continuous("Year", limits = c(1995, 2045),
                             breaks = seq(2000, 2040, 10), labels = c("2000", "'10", "'20", "'30", "'40")) +
          scale_y_continuous("Frontier mortality rate\n(per 100K, log scale)", trans = "log",
                             limits = ylims, breaks = log.breaks, labels = log.labels) +
          scale_color_manual("Frontier estimation stage", values = colors$colors) +
          scale_fill_manual("Frontier estimation stage", values = colors$fills) +
          theme(legend.position = "bottom") +
          guides(color = guide_legend(title.position = "bottom",
                                      override.aes = list(pch = c(21, NA, 21))))

        if(k == "Males"){
          if(l == 1){
            rows[[as.character(l)]] <- rows[[as.character(l)]] +
              labs(title = paste(id, paste0(country, ": ", i)), subtitle = k) +
              theme(axis.title.x = element_blank())
          }else{
            rows[[as.character(l)]] <- rows[[as.character(l)]] +
              theme(axis.title.x = element_blank())
          }
        }else{
            if(l == 1){
              rows[[as.character(l)]] <- rows[[as.character(l)]] +
                labs(subtitle = k) +
                theme(axis.title.x = element_blank())
            }
        }

      }

      rows[["space"]] <- ""
      Rows <- list("1" = rows[["1"]], "space" = rows[["space"]], "2" = rows[["2"]])

      # Combining rows
      if(k == "Males"){
        panels[[k]] <- ggarrange(plotlist = Rows, ncol = 1, align = "hv", heights = c(1, -0.2, 1),
                                 legend = "none")
      }else{
        panels[[k]] <- ggarrange(plotlist = Rows, ncol = 1, align = "hv", heights = c(1, -0.2, 1),
                                 common.legend = TRUE, legend = "bottom")
      }

    }

    # Combining panels
    grobs[[i]] <- ggarrange(plotlist = panels, ncol = 1, align = "hv")

  }

  filename <- gsub("ISO", j, "3-2_country-scaling_ISO.pdf")
  saveGGplot(x = grobs, filename, folder = "output/figures",
             width = 12, height = 12, multipage = TRUE)

}
