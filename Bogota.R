

sarahLoad(c("frontier_lagged", "region"), folder = "data/processed")

temp1 <- region %>%
  filter(region == "Latin America & Caribbean",
         causename == "Cardiovascular diseases",
         sex == 1, age >= 30)

temp2 <- left_join(temp1, frontier_lagged, by = c("year", "age", "ghecause", "causename")) %>%
  filter(year >= 2000, year <= 2040, age < 85) %>%
  mutate(delta = dths_rate - frontier,
         age2 = makeMathersAgeGroup(age),
         age = makeAgeGroup(age))

color_info <- temp2 %>%
  dplyr::select(age2, age) %>% unique() %>%
  group_by(age2) %>%
  dplyr::summarize(n = n(), age = paste0(age, collapse = "; "))
colors <- groupColor(n = color_info$n,
                     names = unlist(str_split(paste(color_info$age, collapse = "; "), "; ")),
                     color.and.fill = TRUE)

figure <- ggplot(temp2) +
  facet_wrap(~ age, ncol = 3) +
  geom_point(data = subset(temp2, year <= 2019),
             aes(x = year, y = dths_rate, color = as.factor(age))) +
  geom_line(aes(x = year, y = dths_rate, color = as.factor(age))) +
  geom_line(aes(x = year, y = frontier), size = 0.75, color = "black") +
  labs(title = "Latin America & the Caribbean vs. the Frontier",
       subtitle = "Cardiovascular diseases") +
  scale_x_continuous("Year") +
  scale_y_continuous("Mortality rate (per 100K, log scale)", trans = "log",
                     breaks = log.breaks, labels = log.labels, limits = c(0.1, NA)) +
  scale_color_manual("Age", values = colors$colors) +
  guides(color = "none")

saveGGplot(figure, "Bogota-LAC-frontier.pdf", folder = "output/figures", width = 7, height = 7)

