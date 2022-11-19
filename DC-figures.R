
### DC Meeting Figures



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()



# 4 Fig 3 R bar -----------------------------------------------------------

sarahLoad("region_calculations", folder = "data/processed")

figure_name <- "DC_Fig3.pdf"

ggdata <- region_calculations %>%
  filter(causename != "Communicable, maternal, perinatal and nutritional conditions",
         mece == "Level 2", lambda == 5, epsilon == 1,
         year %in% c(2019, 2030, 2040)) %>%
  left_join(cause_hierarchy %>% select(ghecause, main_causename), by = "ghecause") %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females"),
         main_causename = factor(main_causename, levels = c("Noncommunicable diseases", "Injuries")),
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
  fills <- c(colorFunct(7), greys[2], greys[1])
  colors <- darken(fills, 0.3)
  colors <- list(colors = setNames(colors, names),
                 fills = setNames(fills, names))

  panels[[id]] <- ggplot(ggdata2) +
    facet_wrap(~ wrapper(region, 20), nrow = 1) +
    geom_col(aes(x = as.factor(year), y = R_bar, fill = causename, color = causename),
             position = "fill") +
    labs(title = paste(id, i)) +
    scale_x_discrete("") +
    scale_y_continuous(bquote(bar(R)), expand = expansion(mult = c(0, 0.05))) +
    scale_color_manual("Level 2\nNCD and injury causes", values = colors$colors) +
    scale_fill_manual("Level 2\nNCD and injury causes", values = alpha(colors$fills, 0.9)) +
    theme(axis.line = element_blank(), axis.ticks.x = element_blank(),
          legend.key.height = unit(25, "pt"), legend.position = "bottom",
          panel.grid.major.x = element_blank()) +
    guides(color = guide_legend(ncol = 1, title.position = "top"))

  if(i == "Females"){
    panels[[id]] <- panels[[id]] + theme(axis.title.x = element_blank())
  }

}

figure <- ggarrange(plotlist = panels, ncol = 1, align = "hv", common.legend = TRUE, legend = "right")
saveGGplot(figure, figure_name, "output/figures", width = 14, height = 10)



# 5 Fig 4 economic values -------------------------------------------------

figure_name <- "DC_Fig4.pdf"

sarahLoad("region_calculations", folder = "data/processed")
chang <- read.csv("data/input/chang_valuation-by-region.csv") %>%
  mutate(region = gsub("\n", " ", wb.region)) %>%
  select(region, year, b_log_1yr)

region.levels <- rev(sort(unique(region_calculations$region)))

ggdata <- region_calculations %>%
  filter(causename != "Communicable, maternal, perinatal and nutritional conditions",
         ghecause %in% c(0, 1100, 610, 1510),
         year == 2019, lambda == 5, epsilon == 1, mece %in% c("Level 1", "Level 2")) %>%
  left_join(chang, by = c("region", "year")) %>%
  mutate(sex = ifelse(sex == 1, "Males", "Females"),
         sex = factor(sex, levels = c("Males", "Females")),
         causename = factor(wrapper(causename, 35), levels = rev(wrapper(causename.levels, 35))),
         pct = R_bar * b_log_1yr) %>%
  group_by(region, year, sex, ghecause, causename) %>%
  slice_head(n = 1) %>%
  dplyr::select(region, year, sex, ghecause, causename, pct) %>%
  group_by(region, year, ghecause, causename) %>%
  mutate(lower = min(pct),
         upper = max(pct)) %>%
  ungroup()

table <- ggdata %>%
  select(-c(lower, upper, year, ghecause)) %>%
  mutate(pct = paste0(format(round(pct * 100, 1), nsmall = 1), "%")) %>%
  pivot_wider(names_from = sex, values_from = pct) %>%
  arrange(causename, region) %>%
  group_by(causename) %>%
  group_modify(~add_row(.x, .before = 0)) %>%
  mutate(region = ifelse(is.na(region), as.character(causename), paste0("   ", region))) %>%
  ungroup() %>% select(-causename)

write.csv(table, file = "output/tables/DC_Fig4-table.csv", row.names = FALSE, na = "")


ggdata_segments <- ggdata %>%
  select(region, year, ghecause, causename, lower, upper) %>%
  unique()

colors <- colorFunct(length(unique(ggdata$causename)),
                     unique(ggdata$causename), color.and.fill = TRUE)

greys <- ggsci::pal_material("grey")(9)[c(3, 5, 9)]
fills <- c(greys[2], colorFunct(7)[c(4, 1)])
colors <- darken(fills, 0.3)

shapes <- c("Females" = 21, "Males" = 24)

figure <- ggplot(ggdata) +
  facet_wrap(~ region, ncol = 1) +
  geom_segment(data = ggdata_segments,
               aes(y = causename, yend = causename, x = lower, xend = upper, color = causename),
               size = 1, alpha = 0.5) +
  geom_point(aes(x = pct, y = causename, color = causename, fill = causename, shape = sex), size = 2.5) +
  geom_label(data = ggdata_segments, aes(x = lower-0.001, y = causename,
                                         label = paste0(format(round(lower * 100, 1), nsmall = 1, trim = TRUE), "%")),
             hjust = 1, family = "Barlow", fill = alpha("white", 0.8), label.size = 0) +
  geom_label(data = ggdata_segments, aes(x = upper+0.001, y = causename,
                                         label = paste0(format(round(upper * 100, 1), nsmall = 1, trim = TRUE), "%")),
             hjust = 0, family = "Barlow", fill = "white", label.size = 0) +
  scale_x_continuous("Percent (%) of annual income", expand = expansion(mult = c(0.06, 0.06)),
                     breaks = pretty_breaks(5), labels = label_percent()) +
  scale_y_discrete("") +
  scale_color_manual("", values = colors) +
  scale_fill_manual("", values = fills) +
  scale_shape_manual("", values = shapes) +
  theme(axis.title.y = element_blank(), axis.line.y = element_blank(),
        legend.position = "bottom",
        panel.grid.minor.x = element_line(color = alpha("black", 0.2), size = 0.2)) +
  guides(color = "none", fill = "none",
         shape = guide_legend(override.aes = list(shape = c(19, 17))))

saveGGplot(figure, figure_name, "output/figures", width = 10, height = 12)
