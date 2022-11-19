
### 4.1 Region aggregation



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("country_scaled", "population"), folder = "data/processed")



# 2 Back-calculating deaths from mortality rates --------------------------

# Back calculating deaths for projected years
temp1 <- left_join(country_scaled, population, by = c("iso3", "year", "age", "sex")) %>%
  mutate(dths = (dths_rate / 100000) * pop)

# Summing deaths and population by region and calculating mortality rates
region <- temp1 %>%
  group_by(region, year, sex, age, ghecause, causename) %>%
  dplyr::summarize_at(vars(dths, pop), ~ sum(.), .groups = "drop") %>%
  mutate(dths_rate = (dths / pop) * 100000) %>%
  ungroup()

# Checking scaling
reference <- region %>%
  filter(ghecause == 0) %>%
  dplyr::select(region, year, sex, age, reference = dths_rate)
levels <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
concerns <- list()
for(i in levels){

  check <- region %>%
    left_join(cause_hierarchy %>% select(ghecause, mece = !!as.name(i)),
              by = "ghecause") %>%
    filter(mece) %>%
    group_by(region, year, sex, age) %>%
    dplyr::summarize(lower_summed = sum(dths_rate), .groups = "drop") %>%
    left_join(reference, by = c("region", "year", "sex", "age")) %>%
    mutate(sf = reference / lower_summed)

  concern <- check %>%
    filter(sf < 0.99 | sf > 1.01) %>%
    arrange(desc(sf))

  if(nrow(concern) > 0){
    warning(paste("Concerning scaling factors:", names(levels[levels == i])))
    concerns[[names(levels[levels == i])]] <- concern
  }
}


# __ + region -------------------------------------------------------------
sarahSave("region", folder = "data/processed")


# 3 GRAPHING --------------------------------------------------------------

# Formatting data for graphing
ggdata <- region %>%
  filter(age >= 30) %>%
  left_join(cause_hierarchy %>% select(ghecause, prefix), by = "ghecause") %>%
  mutate(age2 = makeDisplayAgeGroup(age)) %>%
  mutate(age = makeAgeGroup(age),
         sex = ifelse(sex == 1, "Males", "Females"),
         dths_rate = ifelse(dths_rate < 0.1, 0, dths_rate)) %>%
  mutate(zero = dths_rate == 0) %>%
  select(region, year, sex, age2, age, prefix, ghecause, causename, dths_rate, zero) %>%
  arrange(region, prefix, year, sex, age2, age)

color.info <- groupColorInfo(ggdata, "age2", "age")
colors <- groupColor(color.info$n, color.info$names, color.and.fill = TRUE)
shapes <- c(21, NA)

for(j in unique(ggdata$region)){
  grobs <- list()
  for(i in unique(ggdata$causename)){

    panels <- list()
    ggdata2 <- ggdata %>% filter(region == j, causename == i)

    id <- ids[which(unique(ggdata$causename) == i)]

    ylims <- ggRange(ggdata2$dths_rate)
    if(ylims[1] <= 0){
      ylims <- c(0.1, ylims[2])
    }

    base <- ggplot(ggdata2) +
      facet_grid(cols = vars(age2), rows = vars(sex)) +
      geom_line(aes(x = year, y = dths_rate, color = age), size = 0.5) +
      scale_x_continuous("", breaks = seq(2000, 2040, 10),
                         labels = c("2000", "'10", "'20", "'30", "'40")) +
      scale_color_manual("Age", values = colors$colors) +
      scale_fill_manual("Age", values = colors$fills) +
      scale_shape_manual(values = shapes) +
      theme(axis.title.x = element_blank(),
            panel.grid.minor.y = element_blank(), legend.position = "bottom") +
      guides(color = guide_legend(title.position = "top", nrow = 3),
             fill = guide_legend(title.position = "top", nrow = 3),
             linetype = guide_legend(title.position = "top", nrow = 3),
             shape = "none")

    panels[["standard"]] <- base +
      labs(title = paste(id, paste0(j, ": ", i)),
           subtitle = "Standard scale") +
      scale_y_continuous("Mortality rate (per 100K)", limits = ylims,
                         breaks = pretty_breaks(5),
                         labels = label_number(scale_cut = cut_long_scale()))

    # Spacer
    panels[["space"]] <- ""

    # Log scale
    panels[["log"]] <- base +
      labs(subtitle = "Log scale") +
      scale_y_continuous("Mortality rate (per 100K)", trans = "log", limits = ylims,
                         breaks = log.breaks, labels = log.labels)

    # Combining panels
    figure <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.1, 1),
                        common.legend = TRUE, legend = "bottom")

    grobs[[i]] <- as_grob(figure)

  }

  lookUp <- function(x, look.up.vector){
    y <- unname(look.up.vector[x])
    return(y)
  }
  region.abb <- c("China" = "CHN", "Euroasia & Mediterranean" = "EM", "High-income" = "HI", "India" = "IND",
                  "Latin America & Caribbean" = "LAC", "Sub-Saharan Africa" = "SSA")
  region <- lookUp(j, region.abb)

  filename <- gsub("REGION", region, "1-1_region-aggregation_REGION.pdf")
  saveGGplot(grobs, filename, folder = "output/figures",
             width = 11, height = 12, multipage = TRUE)

}
