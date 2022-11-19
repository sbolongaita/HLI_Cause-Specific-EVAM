
### 3.3 Country calculations



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad(c("country_info", "country_scaled", "frontier_lagged", "population"), folder = "data/processed")


# 2 Mortality differential ------------------------------------------------

# Prepping country data
country_scaled %<>%
  mutate(sex_match = ifelse(causename %in% sex.specific, sex, NA)) %>%
  dplyr::select(iso3, year, age, sex_match, sex, ghecause, causename, dths_rate) %>%
  arrange(iso3, year, age, ghecause, sex)

# Prepping frontier data
frontier_lagged %<>%
  dplyr::select(year, age, sex_match = sex, ghecause, causename, frontier) %>%
  arrange(year, age, ghecause, sex_match)

# Combining country and frontier data
data <- inner_join(country_scaled, frontier_lagged, by = c("year", "age", "sex_match", "ghecause", "causename")) %>%
  mutate_at(vars(dths_rate, frontier), ~ . /100000) %>%
  dplyr::select(-sex_match) %>%
  arrange(iso3, ghecause, age, sex, year)

containsNA(data)


# 3 CALCULATING VARIABLES -------------------------------------------------

# * delta -----------------------------------------------------------------
# Difference between frontier mortality and country mortality
delta <- data %>%
  mutate(delta = exp(-1 * frontier) - exp(-1 * dths_rate)) %>%
  mutate(delta = ifelse(delta < 0, 0, delta))

containsNA(delta)

# * alpha -----------------------------------------------------------------
# Population weight
population %<>%
  group_by(iso3, year) %>%
  mutate(alpha = pop / sum(pop)) %>%
  select(iso3, year, age, sex, pop, alpha)

alpha <- delta %>% left_join(population, by = c("iso3", "year", "age", "sex"))

containsNA(alpha)


# * p2 --------------------------------------------------------------------
mece <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
for(i in seq_along(mece)){
  level <- names(mece[i])
  out <- p1 %>%
    left_join(cause_hierarchy %>% select(ghecause, mece = !!as.name(mece[i])),
              by = "ghecause") %>%
    filter(mece) %>%
    group_by(iso3, year, age, sex, lambda) %>%
    dplyr::mutate(mece = level,
                  p2 = delta / sum(delta)) %>%
    ungroup()
  if(i == 1){
    p2 <- out
  }else{
    p2 <- bind_rows(p2, out)
  }
}

containsNA(p2)

p2 %<>%
  mutate(p2 = ifelse(is.nan(p2), 0, p2)) %>%
  arrange(iso3, year, age, ghecause, sex)

containsNA(p2)



# Multiply by country alpha for regional level and diego's b

# ! Not age-specific from this point forward ------------------------------
# * R ---------------------------------------------------------------------
R <- p2 %>%
  mutate(r = alpha * p1 * p2) %>%
  group_by(iso3, year, sex, ghecause, causename, lambda, mece) %>%
  dplyr::summarize(R = sum(r), .groups = "drop")

containsNA(R)

# * R_bar -----------------------------------------------------------------
R_bar <- R %>%
  group_by(iso3, year, sex, lambda, mece) %>%
  dplyr::mutate(R_bar = R / sum(R)) %>%
  ungroup()

containsNA(R_bar)

# Checking R_bar sums to 1
check <- R_bar %>%
  group_by(iso3, year, sex, lambda, mece) %>%
  dplyr::summarize(R_bar = sum(R_bar), .groups = "drop") %>%
  filter(round(R_bar, 0.01) > 1)
if(nrow(check) > 0){
  print(check)
}


# Saving
country_calculations <- mult
sarahSave("country_calculations", folder = "data/processed")
sarahSave("country_calculations", folder = "output/data")



# # 4 GRAPHING --------------------------------------------------------------
#
# # Initializing figures list
# figures <- list()
#
# # Establishing figure folder
# script <- "510-country_calculations"
# folder <- paste("figures", script, sep = "/")
# make_folder(folder)
#
# sarah_load("country_info")
#
# ggdata <- V %>%
#   filter(iso3 == "AFG", epsilon == 1, sex == 1) %>%
#   mutate(causename = factor(causename, levels = causename.levels)) %>%
#   arrange(iso3, year, sex, causename) %>%
#   ungroup()
# ggdata$main <- get_cause_info(ggdata$ghecause, return = "main_causename")
#
# n_colors <- ggdata %>% dplyr::select(main, causename) %>% unique() %>% group_by(main) %>% summarize(n = n())
# names <- ggdata %>% dplyr::select(main, causename) %>% unique() %>% arrange(causename) %>% pull(causename)
#
# colors <- group_color_funct(n_colors$n, names = names)
#
# ggplot(ggdata) +
#   geom_col(aes(x = mece, y = R_bar, fill = causename, color = causename)) +
#   facet_wrap(~ year) +
#   scale_fill_manual("", values = colors$fills) +
#   scale_color_manual("", values = colors$colors)
#
# for(REGION in unique(ggdata$region)){
#   for(MECE in unique(ggdata$mece)){
#
#     ggdata2 <- ggdata
#
#
#
#   }
# }
#
#
#
#
# # REGION_Level-2.png ------------------------------------------------------
#
# # Figure base name
# figure_name_base <- "REGION_R_lvl-2_lambda-LAMBDA.png"
#
# # Prepping data
# temp1 <- R_bar_lvl2 %>%
#   filter(year %in% seq(2020, 2040, 10)) %>%
#   left_join(cause_hierarchy, by = c("ghecause", "causename")) %>%
#   mutate(year = as.factor(year),
#          sex = ifelse(sex == 1, "Males", "Females"))
#
# causename_levels <- temp1 %>%
#   mutate(group = factor(group, levels = group_levels)) %>%
#   group_by(group, ghecause, causename) %>%
#   dplyr::summarize(R = mean(R), .groups = "drop") %>%
#   arrange(group, R) %>% pull(causename) %>% unique()
#
# ggdata <- temp1 %>%
#   mutate(group = factor(group, levels = group_levels)) %>%
#   mutate(causename = factor(causename, levels = causename_levels)) %>%
#   arrange(region, year, sex, group, causename)
#
# # Graphing parms
# fills1 <- color_funct(n_colors = length(unique(ggdata$causename[ggdata$group == "Injuries"])),
#                       palette_type = "cartography", palette = "pink.pal")
# fills2 <- color_funct(n_colors = length(unique(ggdata$causename[ggdata$group == "Noncommunicable diseases"])),
#                       palette_type = "cartography", palette = "blue.pal")
# fills3 <- color_funct(n_colors = length(unique(ggdata$causename[ggdata$group == "Communicable, maternal, perinatal and nutritional conditions"])),
#                       palette_type = "cartography", palette = "green.pal")
# fills <- setNames(c(fills1, fills2, fills3),
#                   c(unique(ggdata$causename[ggdata$group == "Injuries"]),
#                     unique(ggdata$causename[ggdata$group == "Noncommunicable diseases"]),
#                     unique(ggdata$causename[ggdata$group == "Communicable, maternal, perinatal and nutritional conditions"])))
# colors <- setNames(darken(fills, 0.3),
#                    c(unique(ggdata$causename[ggdata$group == "Injuries"]),
#                      unique(ggdata$causename[ggdata$group == "Noncommunicable diseases"]),
#                      unique(ggdata$causename[ggdata$group == "Communicable, maternal, perinatal and nutritional conditions"])))
#
# # Graphing
# for(j in unique(ggdata$lambda)[2]){
#
#   ggdata2 <- ggdata %>% filter(lambda == j)
#
#   for(i in unique(ggdata$region)){
#
#     figure_name <- gsub("REGION", to_slug(i), gsub("LAMBDA", j, figure_name_base))
#
#     ggdata3 <- ggdata2 %>% filter(region == i)
#
#     figures[[figure_name]] <- ggplot(ggdata3, aes(x = as.factor(year), y = R, fill = causename, color = causename)) +
#       geom_bar(position = "fill",stat = "identity") +
#       facet_wrap(~ sex, ncol = 2) +
#       labs(title = paste0(i, ": Proportion of R by cause"), subtitle = bquote(lambda==.(j))) +
#       scale_x_discrete("Year") +
#       scale_y_continuous("Proportion of R", expand = expansion(mult = c(0, 0.01))) +
#       scale_fill_manual("", values = fills) +
#       scale_color_manual("", values = colors) +
#       theme(axis.line.x = element_blank(), axis.line.y = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(),
#             legend.position = "right",
#             panel.grid.major.x = element_blank()) +
#       guides(color = guide_legend(ncol = 1),
#              fill = guide_legend(ncol = 1))
#
#     # Saving figure
#     ggsave(figures[[figure_name]], filename = paste(folder, figure_name, sep = "/"), device = "png",
#            dpi = 300, scale = 9, width = 1.25, height = 1, limitsize = TRUE); print(figure_name)
#
#   }
# }
#
#
# # REGION_Level-3.png ------------------------------------------------------
#
# # Figure base name
# figure_name_base <- "REGION_R_lvl-3.png"
#
# # Prepping data
# temp1 <- R_bar_lvl3 %>%
#   filter(year %in% seq(2020, 2040, 10),
#          lambda == 5) %>%
#   left_join(cause_hierarchy, by = c("ghecause", "causename")) %>%
#   mutate(year = as.factor(year),
#          sex = ifelse(sex == 1, "Males", "Females"))
#
# # R_lvl3 <- temp1
# # sarah_save("R_lvl3", save_both = TRUE)
#
# causename_levels <- temp1 %>%
#   mutate(group = factor(group, levels = group_levels)) %>%
#   group_by(group, ghecause, causename) %>%
#   dplyr::summarize(R = mean(R), .groups = "drop") %>%
#   arrange(group, R) %>% pull(causename) %>% unique()
#
# ggdata <- temp1 %>%
#   mutate(group = factor(group, levels = group_levels)) %>%
#   mutate(causename = factor(causename, levels = causename_levels)) %>%
#   arrange(region, year, sex, group, causename)
#
# # Graphing parms
# fills1 <- color_funct(n_colors = length(unique(ggdata$causename[ggdata$group == "Injuries"])),
#                       palette_type = "cartography", palette = "pink.pal")
# fills2 <- color_funct(n_colors = length(unique(ggdata$causename[ggdata$group == "Noncommunicable diseases"])),
#                       palette_type = "cartography", palette = "blue.pal")
# fills3 <- color_funct(n_colors = length(unique(ggdata$causename[ggdata$group == "Communicable, maternal, perinatal and nutritional conditions"])),
#                       palette_type = "cartography", palette = "green.pal")
# fills <- setNames(c(fills1, fills2, fills3),
#                   c(unique(ggdata$causename[ggdata$group == "Injuries"]),
#                     unique(ggdata$causename[ggdata$group == "Noncommunicable diseases"]),
#                     unique(ggdata$causename[ggdata$group == "Communicable, maternal, perinatal and nutritional conditions"])))
# colors <- setNames(darken(fills, 0.3),
#                    c(unique(ggdata$causename[ggdata$group == "Injuries"]),
#                      unique(ggdata$causename[ggdata$group == "Noncommunicable diseases"]),
#                      unique(ggdata$causename[ggdata$group == "Communicable, maternal, perinatal and nutritional conditions"])))
#
# # Graphing
# for(i in unique(ggdata$region)){
#
#   figure_name <- gsub("REGION", to_slug(i), figure_name_base)
#
#   ggdata2 <- ggdata %>% filter(region == i)
#
#   figures[[figure_name]] <- ggplot(ggdata2, aes(x = as.factor(year), y = R, fill = causename, color = causename)) +
#     geom_bar(position = "fill",stat = "identity") +
#     facet_wrap(~ sex, ncol = 2) +
#     labs(title = paste0(i, ": Proportion of R by cause"), subtitle = bquote(~"Level 3 causes"~(lambda==5))) +
#     scale_x_discrete("Year") +
#     scale_y_continuous("Proportion of R", expand = expansion(mult = c(0, 0.01))) +
#     scale_fill_manual("", values = fills) +
#     scale_color_manual("", values = colors) +
#     theme(axis.line.x = element_blank(), axis.line.y = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(),
#           legend.position = "right",
#           panel.grid.major.x = element_blank()) +
#     guides(color = guide_legend(ncol = 1),
#            fill = guide_legend(ncol = 1))
#
#   # Saving figure
#   ggsave(figures[[figure_name]], filename = paste(folder, figure_name, sep = "/"), device = "png",
#          dpi = 300, scale = 9, width = 1.5, height = 1, limitsize = TRUE); print(figure_name)
#
# }
#
#
#
# # REGION_Level-2_lambda-comparison.png -----------------------------------
#
# # Figure base name
# figure_name_base <- "REGION_R_lvl-2_lambda-comparison.png"
#
# # Prepping data
# temp1 <- R_bar_lvl2 %>%
#   filter(year %in% seq(2020, 2040, 10)) %>%
#   left_join(cause_hierarchy, by = c("ghecause", "causename")) %>%
#   mutate(year = as.factor(year),
#          sex = ifelse(sex == 1, "Males", "Females"))
#
# causename_levels <- temp1 %>%
#   mutate(group = factor(group, levels = group_levels)) %>%
#   group_by(group, ghecause, causename) %>%
#   dplyr::summarize(R = mean(R), .groups = "drop") %>%
#   arrange(group, R) %>% pull(causename) %>% unique()
#
# ggdata <- temp1 %>%
#   mutate(group = factor(group, levels = group_levels)) %>%
#   mutate(causename = factor(causename, levels = causename_levels)) %>%
#   arrange(region, year, sex, group, causename)
#
# # Graphing parms
# fills1 <- color_funct(n_colors = length(unique(ggdata$causename[ggdata$group == "Injuries"])),
#                       palette_type = "cartography", palette = "pink.pal")
# fills2 <- color_funct(n_colors = length(unique(ggdata$causename[ggdata$group == "Noncommunicable diseases"])),
#                       palette_type = "cartography", palette = "blue.pal")
# fills3 <- color_funct(n_colors = length(unique(ggdata$causename[ggdata$group == "Communicable, maternal, perinatal and nutritional conditions"])),
#                       palette_type = "cartography", palette = "green.pal")
# fills <- setNames(c(fills1, fills2, fills3),
#                   c(unique(ggdata$causename[ggdata$group == "Injuries"]),
#                     unique(ggdata$causename[ggdata$group == "Noncommunicable diseases"]),
#                     unique(ggdata$causename[ggdata$group == "Communicable, maternal, perinatal and nutritional conditions"])))
# colors <- setNames(darken(fills, 0.3),
#                    c(unique(ggdata$causename[ggdata$group == "Injuries"]),
#                      unique(ggdata$causename[ggdata$group == "Noncommunicable diseases"]),
#                      unique(ggdata$causename[ggdata$group == "Communicable, maternal, perinatal and nutritional conditions"])))
#
# # Graphing
# for(i in unique(ggdata$region)){
#
#   figure_name <- gsub("REGION", to_slug(i), figure_name_base)
#
#   ggdata2 <- ggdata %>% filter(region == i)
#
#   panels <- list()
#   for(j in unique(ggdata$sex)){
#
#     ggdata3 <- ggdata2 %>% filter(sex == j)
#
#     panels[[j]] <- ggplot(ggdata3, aes(x = as.factor(lambda), y = R, fill = causename, color = causename)) +
#       geom_bar(position = "fill",stat = "identity") +
#       facet_wrap(~ year, ncol = 3) +
#       scale_x_discrete(bquote(lambda)) +
#       scale_y_continuous("Proportion of R", expand = expansion(mult = c(0, 0.01))) +
#       scale_fill_manual("", values = fills) +
#       scale_color_manual("", values = colors) +
#       theme(axis.line.x = element_blank(), axis.line.y = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(),
#             legend.position = "bottom",
#             panel.grid.major.x = element_blank()) +
#       guides(color = guide_legend(ncol = 1),
#              fill = guide_legend(ncol = 1))
#
#     if(j == "Females"){
#       panels[[j]] <- panels[[j]] +
#         labs(title = paste0(i, ": Proportion of R by level 2 cause"), subtitle = j)
#     }else{
#       panels[[j]] <- panels[[j]] +
#         labs(title = "", subtitle = j)
#     }
#
#   }
#   # Combining figures
#   figures[[figure_name]] <- ggarrange(plotlist = panels, ncol = 1, align = "hv",
#                                       common.legend = TRUE, legend = "right")
#
#   # Saving figure
#   ggsave(figures[[figure_name]], filename = paste(folder, figure_name, sep = "/"), device = "png",
#          dpi = 300, scale = 9, width = 1.5, height = 1, limitsize = TRUE); print(figure_name)
#
# }
# #
#
#
#
# # 5 END -------------------------------------------------------------------
#
# # Tidying environment and notifying the end of the script
# notifyScript(); tidy()
