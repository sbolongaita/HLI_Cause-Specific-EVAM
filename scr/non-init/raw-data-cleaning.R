
library(dplyr)
library(haven)
library(tidyr)

# This script cleans the raw data at a basic, perfunctory level. The raw
# data is not uploaded to DropBox for the sake of user simplicity and
# because many of the files are too large. The outputs of this script populate
# the "data/input" folder and are used in all subsequent analysis scripts.

# Regions
temp1 <- read.csv("data/raw/HLI_regions.csv", as.is = TRUE) %>%
  dplyr::select(iso3, country = wbcountryname, region = cihdisplay) %>%
  mutate(region = gsub("Euroasia", "Eurasia", region))
temp2 <- temp1 %>% select(iso3, country) %>%
  mutate(region = "World")
region <- bind_rows(temp1, temp2) %>%
  arrange(region, iso3)
write.csv(region, file = "data/input/region.csv", row.names = FALSE)

# Population
temp1 <- read_dta("data/raw/UNPOP_population-NEW.dta") %>%
  zap_formats() %>% zap_label() %>%
  filter(year >= 2000, year <= 2050) %>%
  dplyr::select(iso3, year, sex, starts_with("p")) %>%
  pivot_longer(cols = starts_with("p"), names_to = "age", values_to = "pop") %>%
  mutate(age = as.integer(gsub("p", "", age)))
population <- temp1 %>%
  arrange(iso3, year, sex, age)
write.csv(population, file = "data/input/population.csv", row.names = FALSE)

# GNI per capita
temp1 <- read.csv("data/raw/WDI_GNI-per-capita.csv", na.strings = c("..", ""), as.is = TRUE) %>%
  filter(!is.na(Time), nchar(Time) <= 4) %>%
  mutate(year = as.integer(Time)) %>%
  filter(year >= 2019) %>%
  select(iso3 = Country.Code, year,
         "Constant 2017 international dollars" = "GNI.per.capita..PPP..constant.2017.international.....NY.GNP.PCAP.PP.KD.",
         "Current international dollars" = "GNI.per.capita..PPP..current.international.....NY.GNP.PCAP.PP.CD.") %>%
  pivot_longer(cols = c("Constant 2017 international dollars", "Current international dollars"),
               names_to = "currency", values_to = "gni.pc")
gni <- temp1 %>%
  arrange(iso3, year, currency)
write.csv(gni, file = "data/input/gni.csv", row.names = FALSE)

# GHE
temp1 <- read_dta("data/raw/GHE_2000-2019.dta") %>%
  zap_formats() %>% zap_label() %>%
  mutate(causename = ifelse(ghecause == 0, "All causes", causename)) %>%
  select(iso3, year, sex, age, ghecause, causename, dths)
temp2 <- temp1 %>% arrange(year, iso3, sex, age, ghecause)
temp3 <- split(temp2, temp2$year)
for(i in names(temp3)){
  filename <- paste0("data/input/ghe_", i, ".csv")
  write.csv(temp3[[i]], file = filename, row.names = FALSE)
  notify(filename)
}

# Quality
temp1 <- read_dta("data/raw/GHE_quality_scores.dta") %>%
  zap_formats() %>% zap_label() %>%
  select(iso3, quality = category)
quality <- temp1 %>%
  arrange(iso3)
write.csv(quality, file = "data/input/quality.csv", row.names = FALSE)

# Chang country
temp1 <- read.csv("data/raw/Chang_avoidable-deaths-NEW.csv", as.is = TRUE) %>%
  filter(year >= 2000, year <= 2050) %>%
  select(iso3, year, sex, age, dths = deaths, pop = p, mxn, ex, frontier_mxn, frontier_ex, av_mxn, av_dths = av_deaths, ex_gain = potential_ex_gain)
chang_country <- temp1 %>%
  arrange(iso3, year, sex, age)
write.csv(chang_country, file = "data/input/chang_country.csv", row.names = FALSE)

# Chang frontier
temp1 <- read.csv("data/raw/Chang_frontier-life-tables-NEW.csv", as.is = TRUE) %>%
  filter(year >= 2000, year <= 2050) %>%
  select(year, age, mxn, ex)
chang_frontier <- temp1 %>%
  arrange(year, age)
write.csv(chang_frontier, file = "data/input/chang_frontier.csv", row.names = FALSE)

# Chang envelope
temp1 <- read.csv("data/raw/Chang_envelopes-sensitivity-NEW.csv", as.is = TRUE)
temp2 <- temp1 %>%
  mutate(region = gsub("\n", " ", gsub("Euroasia", "Eurasia", wb.region))) %>%
  select(region, iso3 = country, year, age, sex, pop.sex.weight, IE = inc.elasticity.case, DR = disc.rate, BI = base.income.case, b_log_1yr) %>%
  unique()
temp3 <- temp2 %>% mutate(region = "World")
temp4 <- bind_rows(temp2, temp3) %>% arrange(region, iso3)
temp5 <- temp4 %>%
  mutate(scenario = case_when(IE == "B" & DR == 0.03 & BI == "US" ~ "Base",
                              IE == "L" & DR == 0.03 & BI == "US" ~ "Lower income elasticity",
                              IE == "H" & DR == 0.03 & BI == "US" ~ "Higher income elasticity",
                              IE == "B" & DR == 0.00 & BI == "US" ~ "Lower discount rate",
                              IE == "B" & DR == 0.05 & BI == "US" ~ "Higher discount rate",
                              IE == "B" & DR == 0.03 & BI == "OECD" ~ "OECD base income",
                              TRUE ~ NA_character_)) %>%
  filter(!is.na(scenario)) %>%
  mutate(scenario = factor(scenario, levels = c("Base", "Lower income elasticity", "Higher income elasticity", "Lower discount rate",
                                                "Higher discount rate", "OECD base income"))) %>%
  select(scenario, everything()) %>%
  arrange(scenario, region, iso3, year, age, sex)
chang_envelope <- temp5 %>%
  select(scenario, region, iso3, year, age, sex, pop.sex.weight, b_log_1yr)
write.csv(chang_envelope, file = "data/input/chang_envelope.csv", row.names = FALSE)
