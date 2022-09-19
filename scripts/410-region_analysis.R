
### Healthy Longevity Initiative
### 410 - REGION PROJECTION

# This script takes the country projections (output from the
# 8-country_projection.R script) and calculates average mortality rates by region.

# Requires:
# • scripts/0-environment.R (script)
# • output_data/country_recode.Rda (R dataset, output from 6-country_cause_recode.R)

# Returns:
# • "output_data/frontier_projection.Rda" (R dataset)
# • figures/4-frontier_projection (folder of figures)



# 1 ENVIRONMENT -----------------------------------------------------------

# Clearing and loading environment
sourceEnv(); tidy()
scriptName <- "410-region_analysis"

# Loading data
sarahLoad(c("country_info", "country_scaled", "population"))
population %<>%
  dplyr::select(iso3 = iso3.region, year, sex, age, pop)


# 2 BACK CALCULATING DEATHS -----------------------------------------------

# Back calculating deaths for projected years
temp1 <- left_join(country_scaled, population, by = c("iso3", "year", "age", "sex")) %>%
  mutate(dths = (dths_rate / 100000) * pop)

# Summing deaths and population by region and calculating mortality rates
region <- temp1 %>%
  left_join(country_info %>% dplyr::select(iso3, region), by = "iso3") %>%
  group_by(region, year, sex, age, ghecause, causename) %>%
  dplyr::summarize_at(vars(dths, pop), ~ sum(.), .groups = "drop") %>%
  mutate(dths_rate = (dths / pop) * 100000)

# Checking scaling
reference <- ungroup(region) %>% filter(ghecause == 0) %>% dplyr::select(region, year, sex, age, reference = dths_rate)
levels <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
concerns <- list()
for(i in levels){

  region$mece <- getCauseInfo(region$ghecause, return = i)
  check <- region %>%
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
sarahSave("region")


# 3 GRAPHING --------------------------------------------------------------
#
# # Creating figures objects
# folder <- makeFolder(figures)
#
#
# # Formatting data for graphing
# temp1 <- temp1 %>%
#   # mutate(age = makeAgeGroup(age),
#   #        dths_rate_log = ifelse(round(dths_rate, 4) == 0, NA, dths_rate),
#   #        prefix = getCauseInfo(ghecause, return = "prefix")) %>%
#   select(region, year, sex, age, prefix, ghecause, causename, dths_rate, dths_rate_log) %>%
#   arrange(region, prefix, year, sex, age)
#
#
# # * 3.1 All ages ----------------------------------------------------------
#
# # Prepping data for graphing
# ggdata <- temp1
#
# # Graphing parms
# color.info <- groupColorInfo(ggdata, "age2", "age")
# colors <- groupColorFunct(color.info$n, color.info$names, color.and.fill = TRUE)
# linetypes <- c("OLS linear regression" = "solid", "Average mortality rate" = "dashed")
#
# grobs <- list()
# for(i in unique(ggdata$causename)){
#
#   panels <- list()
#   ggdata2 <- ggdata %>% filter(causename == i)
#   id <- ids[which(unique(ggdata$causename) == i)]
#
#   # Standard scale
#   ylims <- ggRange(ggdata2$nmx)
#   panels[["standard"]] <- ggplot(ggdata2) +
#     facet_grid(cols = vars(age2)) +
#     geom_line(data = subset(ggdata2, type == "Projected" & year >= 2020),
#               aes(x = year, y = nmx, color = age, linetype = concern), size = 0.5) +
#     geom_point(data = subset(ggdata2, type == "Observed"),
#                aes(x = year, y = nmx, color = age, fill = age), pch = 21) +
#     labs(title = paste(id, i),
#          subtitle = "Standard scale") +
#     scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
#     scale_y_continuous("Frontier mortality rate (per 100K)", limits = ylims,
#                        breaks = pretty_breaks(5), labels = labelAuto) +
#     scale_color_manual("Age", values = colors$colors) +
#     scale_fill_manual("Age", values = colors$fills) +
#     scale_linetype_manual("Projection method", values = linetypes) +
#     theme(panel.grid.minor.y = element_blank(), legend.position = "bottom") +
#     guides(color = guide_legend(title.position = "top", nrow = 4),
#            linetype = guide_legend(title.position = "top", ncol = 1))
#
#   # Spacer
#   panels[["space"]] <- ""
#
#   # Log scale
#   ylims <- ggRange(ggdata2$nmx_log, log = TRUE)
#   panels[["log"]] <- ggplot(ggdata2) +
#     facet_grid(cols = vars(age2)) +
#     geom_line(data = subset(ggdata2, type == "Projected" & year >= 2020),
#               aes(x = year, y = nmx_log, color = age, linetype = concern), size = 0.5) +
#     geom_point(data = subset(ggdata2, type == "Observed"),
#                aes(x = year, y = nmx_log, color = age, fill = age), pch = 21) +
#     labs(subtitle = "Log scale*") +
#     scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
#     scale_y_continuous("Frontier mortality rate (per 100K)", trans = "log", limits = ylims,
#                        breaks = log.breaks, labels = log.labels) +
#     scale_color_manual("Age", values = colors$colors) +
#     scale_fill_manual("Age", values = colors$fills) +
#     scale_linetype_manual("Projection method", values = linetypes) +
#     theme(axis.title.x = element_blank(),
#           legend.position = "bottom",
#           panel.grid.minor.y = element_blank()) +
#     guides(color = guide_legend(title.position = "top", nrow = 4),
#            linetype = guide_legend(title.position = "top", ncol = 1))
#
#   # Combining panels
#   figure <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.2, 1),
#                       common.legend = TRUE, legend = "bottom") %>%
#     annotate_figure(bottom = text_grob(makeCaption(captions[c("Zero", "Not scaled")]),
#                                        family = "Barlow", size = 11, hjust = 0, x = 0.075))
#
#   grobs[[i]] <- as_grob(figure)
#
# }
#
# saveMultipage(grobs, "frontier_unscaled_all.pdf", width = 11, height = 8.5)
#
#
# # * 3.2 Ages 30 plus ------------------------------------------------------
#
# # Prepping data for graphing
# ggdata <- temp1 %>%
#   filter(age2 %in% levels(temp1$age2)[4:7])
#
# grobs <- list()
# for(i in unique(ggdata$causename)){
#
#   panels <- list()
#   ggdata2 <- ggdata %>% filter(causename == i)
#   id <- ids[which(unique(ggdata$causename) == i)]
#
#   # Standard scale
#   ylims <- ggRange(ggdata2$nmx)
#   panels[["standard"]] <- ggplot(ggdata2) +
#     facet_grid(cols = vars(age2)) +
#     geom_line(data = subset(ggdata2, type == "Projected" & year >= 2020),
#               aes(x = year, y = nmx, color = age, linetype = concern), size = 0.5) +
#     geom_point(data = subset(ggdata2, type == "Observed"),
#                aes(x = year, y = nmx, color = age, fill = age), pch = 21) +
#     labs(title = paste(id, i),
#          subtitle = "Standard scale") +
#     scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
#     scale_y_continuous("Frontier mortality rate (per 100K)", limits = ylims, breaks = pretty_breaks(5),
#                        labels = labelAuto) +
#     scale_color_manual("Age", values = colors$colors) +
#     scale_fill_manual("Age", values = colors$fills) +
#     scale_linetype_manual("Projection method", values = linetypes) +
#     theme(panel.grid.minor.y = element_blank(), legend.position = "bottom") +
#     guides(color = guide_legend(title.position = "top", nrow = 4),
#            linetype = guide_legend(title.position = "top", ncol = 1))
#
#   # Spacer
#   panels[["space"]] <- ""
#
#   # Log scale
#   ylims <- ggRange(ggdata2$nmx_log, log = TRUE)
#   panels[["log"]] <- ggplot(ggdata2) +
#     facet_grid(cols = vars(age2)) +
#     geom_line(data = subset(ggdata2, type == "Projected" & year >= 2020),
#               aes(x = year, y = nmx_log, color = age, linetype = concern), size = 0.5) +
#     geom_point(data = subset(ggdata2, type == "Observed"),
#                aes(x = year, y = nmx_log, color = age, fill = age), pch = 21) +
#     labs(subtitle = "Log scale*") +
#     scale_x_continuous("", breaks = seq(2000, 2040, 10), labels = c("'00", "'10", "'20", "'30", "'40")) +
#     scale_y_continuous("Frontier mortality rate (per 100K)", trans = "log", limits = ylims,
#                        breaks = log.breaks, labels = log.labels) +
#     scale_color_manual("Age", values = colors$colors) +
#     scale_fill_manual("Age", values = colors$fills) +
#     scale_linetype_manual("Projection method", values = linetypes) +
#     theme(axis.title.x = element_blank(),
#           legend.position = "bottom",
#           panel.grid.minor.y = element_blank()) +
#     guides(color = guide_legend(title.position = "top", nrow = 4),
#            linetype = guide_legend(title.position = "top", ncol = 1))
#
#   # Combining panels
#   figure <- ggarrange(plotlist = panels, ncol = 1, align = "hv", heights = c(1, -0.2, 1),
#                       common.legend = TRUE, legend = "bottom") %>%
#     annotate_figure(bottom = text_grob(makeCaption(captions[c("Zero", "Not scaled")]),
#                                        family = "Barlow", size = 11, hjust = 0, x = 0.075))
#
#   grobs[[i]] <- as_grob(figure)
#
# }
#
# saveMultipage(grobs, "frontier_unscaled_30plus.pdf", width = 11, height = 8.5)
#
#


# 5 END -------------------------------------------------------------------

# Tidying environment and notifying the end of the script
notifyScript(); tidy()


