
### Healthy Longevity Initiative
### 125 - BASE DATA CLEANING

# This script cleans the raw data at a basic, perfunctory level. The input
# data for this script is not uploaded to the Github for the sake of user
# simplicity and because some of the files are too large. The outputs of
# script populate the "02-input_data" folder and are used in all subsequent
# analysis scripts.

# Tidying and loading environment
sourceEnv(); tidy()

# Regions
temp1 <- read.csv("02-input_data/Raw/HLI_regions.csv", as.is = TRUE) %>%
  dplyr::select(iso3, country = wbcountryname, region = cihdisplay)
region <- temp1 %>%
  arrange(iso3)
write.csv(region, file = "02-input_data/region.csv", row.names = FALSE)

# Population
temp1 <- read_dta("02-input_data/Raw/UNPOP_population.dta") %>%
  zap_formats() %>% zap_label() %>%
  filter(year >= 2000, year <= 2045) %>%
  dplyr::select(iso3, year, sex, starts_with("p")) %>%
  pivot_longer(cols = starts_with("p"), names_to = "age", values_to = "pop") %>%
  mutate(age = as.integer(gsub("p", "", age)))
population <- temp1 %>%
  arrange(iso3, year, sex, age)
write.csv(population, file = "02-input_data/population.csv", row.names = FALSE)

# GNI per capita
temp1 <- read.csv("02-input_data/Raw/WDI_GNI-per-capita.csv", na.strings = c("..", ""), as.is = TRUE) %>%
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
write.csv(gni, file = "02-input_data/gni.csv", row.names = FALSE)

# GHE
temp1 <- read_dta("02-input_data/Raw/GHE_2000-2019.dta") %>%
  zap_formats() %>% zap_label() %>%
  mutate(causename = ifelse(ghecause == 0, "All causes", causename)) %>%
  select(iso3, year, sex, age, ghecause, causename, dths)
temp2 <- temp1 %>% arrange(year, iso3, sex, age, ghecause)
temp3 <- split(temp2, temp2$year)
for(i in names(temp3)){
  filename <- paste0("02-input_data/ghe_", i, ".csv")
  write.csv(temp3[[i]], file = filename, row.names = FALSE)
}

# Quality
temp1 <- read_dta("02-input_data/Raw/GHE_quality_scores.dta") %>%
  zap_formats() %>% zap_label() %>%
  select(iso3, quality = category)
quality <- temp1 %>%
  arrange(iso3)
write.csv(quality, file = "02-input_data/quality.csv", row.names = FALSE)

# Chang country
temp1 <- read.csv("02-input_data/Raw/Chang_avoidable-deaths.csv", as.is = TRUE) %>%
  filter(year >= 2000, year <= 2045) %>%
  select(iso3, year, sex, age, dths = deaths, pop = p, mxn, ex, frontier_mxn, frontier_ex, av_mxn, av_dths = av_deaths, ex_gain = potential_ex_gain)
chang_country <- temp1 %>%
  arrange(iso3, year, sex, age)
write.csv(chang_country, file = "02-input_data/chang_country.csv", row.names = FALSE)

# Chang frontier
temp1 <- read.csv("02-input_data/Raw/Chang_frontier-life-tables.csv", as.is = TRUE) %>%
  mutate(year = floor(year)) %>%
  filter(year >= 2000, year <= 2045) %>%
  select(year, age, mxn, ex)
chang_frontier <- temp1 %>%
  arrange(year, age)
write.csv(chang_frontier, file = "02-input_data/chang_frontier.csv", row.names = FALSE)
