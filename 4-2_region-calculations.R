
### Healthy Longevity Initiative
### 4-2 Regional calculations



# 1 ENVIRONMENT -----------------------------------------------------------

applyEnv()

# Loading data
sarahLoad(c("country_info", "frontier_scaled", "population", "region"), folder = "data/processed")



# 2 MORTALITY DIFFERENTIAL ------------------------------------------------

# Prepping region data
region %<>%
  mutate(sex_match = ifelse(causename %in% sex.specific, sex, NA)) %>%
  dplyr::select(region, year, age, sex_match, sex, ghecause, causename, dths_rate)

# Prepping frontier data
frontier_scaled %<>%
  dplyr::select(year, age, sex_match = sex, ghecause, causename, frontier)

# Combining country and frontier data
data <- full_join(region, frontier_scaled, by = c("year", "age", "sex_match", "ghecause", "causename")) %>%
  mutate_at(vars(dths_rate, frontier), ~ . /100000) %>%
  dplyr::select(-sex_match)



# 3 CALCULATING VARIABLES -------------------------------------------------

# * delta -----------------------------------------------------------------
# Difference between frontier mortality and country mortality
delta <- data %>%
  mutate(delta = exp(-1 * frontier) - exp(-1 * dths_rate)) %>%
  mutate(delta = ifelse(delta < 0, 0, delta))

# * alpha -----------------------------------------------------------------
# Population weight
alpha <- delta %>%
  left_join(population %>% dplyr::select(region = iso3.region, year, sex, age, alpha),
            by = c("region", "year", "age", "sex"))

# * e,  e_rho,  e40_rho ---------------------------------------------------
# e = Frontier life expectancy; e_rho = Discounted frontier life expectancy
rho = 0.03
temp1 <- read.csv("data/input/chang_frontier.csv", as.is = TRUE) %>%
  mutate(year = floor(year), f.e_rho = ex * 1-(1/(1+rho)^(ex+1))/1-(1/(1+rho))) %>%
  dplyr::select(year, age, f.e = ex, f.e_rho)
e <- alpha %>%
  left_join(temp1, by = c("year", "age"))

# e40_rho = Discounted frontier life expectancy at 40 (used later in V calculations)
e40_rho <- temp1 %>%
  filter(age == 40) %>%
  dplyr::select(year, f.e40_rho = f.e_rho)


# * p1 --------------------------------------------------------------------
lambda = c(1, 5, 10)
for(i in seq_along(lambda)){
  out <- e
  out$lambda <- lambda[i]
  out$p1 <- (1-exp(-1*lambda[i]*out$delta))/(1-exp(-1*lambda[i]))*out$f.e_rho
  if(i == 1){
    p1 <- out
  }else{
    p1 <- bind_rows(p1, out)
  }
}

# * p2 --------------------------------------------------------------------
mece <- c("Level 1" = "mece_lvl1", "Level 2" = "mece_lvl2", "Level 3" = "mece_lvl3")
for(i in seq_along(mece)){
  out <- p1 %>%
    left_join(cause_hierarchy %>% select(ghecause, mece = !!as.name(mece[i])),
              by = "ghecause") %>%
    filter(mece) %>%
    group_by(region, year, age, sex, lambda) %>%
    dplyr::mutate(mece = level,
                  p2 = delta / sum(delta))
  if(i == 1){
    p2 <- out
  }else{
    p2 <- bind_rows(p2, out)
  }
}

# ! Not age-specific from this point forward ------------------------------
# * R ---------------------------------------------------------------------
R <- p2 %>%
  mutate(r = alpha * p1 * p2) %>%
  group_by(region, year, sex, ghecause, causename, lambda, mece) %>%
  dplyr::summarize(R = sum(r), .groups = "drop")

# * R_bar -----------------------------------------------------------------
R_bar <- R %>%
  group_by(region, year, sex, lambda, mece) %>%
  dplyr::mutate(R_bar = R / sum(R))

# Checking R_bar sums to 1
check <- R_bar %>%
  group_by(region, year, sex, lambda, mece) %>%
  dplyr::summarize(R_bar = sum(R_bar), .groups = "drop") %>%
  filter(round(R_bar, 0.01) != 1)
if(nrow(check) > 0){
  print(check)
}

# * Y ---------------------------------------------------------------------
# GNI per capita, 2020
# GNI per capita, 2019 (intl PPP 2017)
Y <- read.csv("data/input/gni.csv", as.is = TRUE) %>%
  filter(currency == "Current international dollars",
         year == 2019,
         iso3 %in% unique(country_info$iso3[country_info$analysis_eligible])) %>%
  dplyr::select(iso3, Y = gni.pc)


# * VSL -------------------------------------------------------------------
# Value of a statistical life
Y_us <- Y$Y[Y$iso3 == "USA"]
VSL_us <- Y_us * 160
temp3 <- expand_grid(Y, epsilon = seq(0.8, 1.2, 0.1)) %>%
  mutate(VSL = VSL_us * (Y/Y_us)^epsilon)

omit.iso3s <- unique(temp3$iso3[is.na(temp3$Y)])

region_pop <- left_join(country_info %>%
                          filter(analysis_eligible) %>% dplyr::select(iso3, region),
                        population %>%
                          filter(year == 2019) %>% ungroup() %>%
                          dplyr::select(iso3 = iso3.region, pop = total_pop) %>% unique(),
                        by = "iso3") %>%
  filter(iso3 %notin% omit.iso3s) %>%
  group_by(region) %>%
  dplyr::mutate(pop_wt = pop / sum(pop)) %>%
  dplyr::select(-pop)

temp4 <- right_join(region_pop, temp3, by = "iso3") %>%
  filter(iso3 %notin% omit.iso3s) %>%
  mutate(Y = pop_wt * Y,
         VSL = pop_wt * VSL) %>%
  group_by(region, epsilon) %>%
  dplyr::summarize(Y = sum(Y),
                   VSL = sum(VSL), .groups = "drop")

VSL <- R_bar %>%
  left_join(temp4, by = "region")


# * VSLY ------------------------------------------------------------------
# Value of a statistical life year
VSLY <- VSL %>%
  left_join(e40_rho, by = "year") %>%
  mutate(VSLY = VSL / f.e40_rho)


# * V, V_bar --------------------------------------------------------------
V <- VSLY %>%
  mutate(V = R * VSLY,
         V_bar = R_bar * VSLY)


# * GDP multiple ----------------------------------------------------------
mult <- V %>%
  mutate(mult = V_bar / Y)

region_calculations <- mult %>% ungroup()

# Saving
sarahSave("region_calculations", folder = "data/processed")
sarahSave("region_calculations", folder = "output/data")



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
#
#
# 5 END -------------------------------------------------------------------
#
# # Tidying environment and notifying the end of the script
# notifyScript(); tidy()
