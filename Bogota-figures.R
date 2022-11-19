
### Bogota Meeting Figures



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()



# 2 Economic value barplot ------------------------------------------------

figure_name <- "Bogota_Slide-15.pdf"

sarahLoad("region_calculations", folder = "data/processed")
chang <- read.csv("data/input/chang_valuation-by-region.csv") %>%
  mutate(region = gsub("\n", " ", wb.region)) %>%
  select(region, year, b_log_1yr)

region.levels <- rev(sort(unique(region_calculations$region)))

ggdata <- region_calculations %>%
  filter(causename %notin% c("Communicable, maternal, perinatal and nutritional conditions"),
         ghecause %in% c(0, 1100, 610, 1510),
         year == 2019, lambda == 5, epsilon == 1, mece %in% c("Level 1", "Level 2")) %>%
  left_join(chang, by = c("region", "year")) %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females"),
         sex = factor(sex, levels = c("Males", "Females")),
         region = wrapper(region, 10),
         causename = factor(wrapper(causename, 35), levels = wrapper(causename.levels, 35)),
         pct = R_bar * b_log_1yr) %>%
  mutate(region = case_when(region == "High-income" ~ "High-\nincome",
                            region == "Sub-Saharan\nAfrica" ~ "Sub-\nSaharan\nAfrica",
                            TRUE ~ region),
         label = paste0(substr(sex, 1, 1), "\n", format(round(pct * 100, 1), nsmall = 1, trim = TRUE), "%"),
         sex.cause = paste(sex, causename)) %>%
  mutate(label = case_when(causename == "Malignant neoplasms" &
                             region %notin% c("China", "High-\nincome", "Latin\nAmerica &\nCaribbean") ~ NA_character_,
                           causename == "Injuries" & sex == "Females" ~ NA_character_,
                           causename == "Injuries" &
                             region %notin% c("Latin\nAmerica &\nCaribbean", "Sub-\nSaharan\nAfrica") ~ NA_character_,
                           TRUE ~ label))
  # group_by(region, year, sex, ghecause, causename)
  # slice_head(n = 1) %>%
  # dplyr::select(region, year, sex, ghecause, causename, pct) %>%
  # group_by(region, year, ghecause, causename) %>%
  # mutate(lower = min(pct),
  #        upper = max(pct)) %>%
  # ungroup()

colors <- colorFunct(length(unique(ggdata$causename)),
                     unique(ggdata$causename), color.and.fill = TRUE)
greys <- ggsci::pal_material("grey")(9)[c(3, 5, 9)]
fills <- c(colorFunct(7)[1], greys[2], colorFunct(7)[4])
colors <- darken(fills, 0.3)

colors <- c(colors, fills)

figure <- ggplot(ggdata) +
  facet_wrap(~ causename, nrow = 1) +
  geom_col(aes(x = region, y = pct, group = sex, fill = sex.cause), position = "dodge") +
  geom_label(aes(x = region, y = pct + 0.001, group = sex, label = label),
            position = position_dodge(width = 0.9), size = 3,
            hjust = 0.5, vjust = 0, family = "Barlow", lineheight = 0.9,
            label.size = 0, label.padding = unit(0.01, "lines")) +
  scale_x_discrete("Region") +
  scale_y_continuous("Percent (%) of annual income", expand = expansion(mult = c(0, 0.12)),
                     breaks = pretty_breaks(5), labels = label_percent()) +
  scale_fill_manual("", values = colors) +
  theme(axis.title.x = element_blank(), axis.line.y = element_blank(),
        legend.position = "bottom",
        panel.grid.minor.x = element_line(color = alpha("black", 0.2), size = 0.2)) +
  guides(color = "none", fill = "none",
         shape = guide_legend(override.aes = list(shape = c(19, 17))))

saveGGplot(figure, figure_name, "output/figures", width = 15, height = 4)
