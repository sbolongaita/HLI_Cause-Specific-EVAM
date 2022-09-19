
### Healthy Longevity Initiative
### 650 - EXPLORATORY FIGURES



# 1 ENVIRONMENT -----------------------------------------------------------

# Tidying and loading environment
sourceEnv(); tidy()
scriptName <- "650-exploratory_figures"

# Creating tables objects
folder <- makeFolder(figures)
figures <- list()



# 2 FigA1 comparison of frontier definitions ------------------------------

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

# dynamicLimits <- function(x){
#   y <- case_when(max(x, na.rm = TRUE) < 10 ~ 10,
#                  max(x, na.rm = TRUE) < 100 ~ 50,
#                  max(x, na.rm = TRUE) < 1000 ~ round_any(max(x, na.rm = TRUE), 500, ceiling),
#                  max(x, na.rm = TRUE) < 10000 ~ round_any(max(x, na.rm = TRUE), 5000, ceiling),
#                  max(x, na.rm = TRUE) < 100000 ~ round_any(max(x, na.rm = TRUE), 50000, ceiling),
#                  max(x, na.rm = TRUE) < 1000000 ~ round_any(max(x, na.rm = TRUE), 500000, ceiling),
#                  max(x, na.rm = TRUE) < 10000000 ~ round_any(max(x, na.rm = TRUE), 5000000, ceiling),
#                  TRUE ~ round_any(max(x, na.rm = TRUE), 50000000, ceiling))
#   return(c(0, y))
# }

grobs.standard <- list()
grobs.log <- list()
for(i in unique(ggdata$causename)){

  ggdata2 <- ggdata %>% filter(causename == i)
  id <- ids[which(unique(ggdata$causename) == i)]

  panels[["standard"]] <- ggplot(ggdata2) +
    facet_grid(cols = vars(stage), rows = vars(age2)) +
    geom_point(data = subset(ggdata2, year < 2020),
               aes(x = year, y = frontier, color = age, fill = age), pch = 21) +
    geom_line(data = subset(ggdata2, year >= 2020),
              aes(x = year, y = frontier, color = age)) +
    labs(title = paste(id, i), subtitle = "Standard scale") +
    scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
    scale_y_continuous("Frontier mortality rate (per 100K)", breaks = pretty_breaks(3), labels = labelAuto) +
    scale_color_manual("", values = colors$colors) +
    scale_fill_manual("", values = colors$fills) +
    theme(axis.line = element_blank(), axis.title.x = element_blank(),
          legend.position = "bottom",
          panel.border = element_rect(color = alpha("black", 0.8), size = 0.2))

  grobs.standard[[id]] <- as_grob(panels[["standard"]])

  panels[["log"]] <- ggplot(ggdata2) +
    facet_grid(cols = vars(stage), rows = vars(age2)) +
    geom_point(data = subset(ggdata2, year < 2020),
               aes(x = year, y = frontier_log, color = age, fill = age), pch = 21) +
    geom_line(data = subset(ggdata2, year >= 2020),
              aes(x = year, y = frontier_log, color = age)) +
    labs(title = paste(id, i), subtitle = "Log scale*",
         caption = makeCaption(captions["Zero"])) +
    scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
    scale_y_continuous("Frontier mortality rate (per 100K)", trans = "log", breaks = log.breaks, labels = log.labels) +
    scale_color_manual("", values = colors$colors) +
    scale_fill_manual("", values = colors$fills) +
    theme(axis.line = element_blank(), axis.title.x = element_blank(),
          legend.position = "bottom",
          panel.border = element_rect(color = alpha("black", 0.8), size = 0.2))

  grobs.log[[id]] <- as_grob(panels[["log"]])

}

saveMultipage(grobs.standard, figure.name = "frontier_process_standard.pdf", width = 12, height = 10)
saveMultipage(grobs.log, figure.name = "frontier_process_log.pdf", width = 12, height = 10)
