
### Healthy Longevity Initiative
### 650 - IN-TEXT VALUES

# This script calculates in-text values for the main manuscript.



# 1 ENVIRONMENT -----------------------------------------------------------

# Tidying and loading environment
tidy(); sourceEnv()
scriptName <- "650-intext_values"

# Loading data
sarahLoad("region_calculations")





CVD <- region_calculations %>%
  filter(region != "Sub-Saharan Africa", year == 2019, causename == "Cardiovascular diseases", lambda == 5, epsilon == 1) %>%
  arrange(R_bar)

communicable <- region_calculations %>%
  filter(region == "Sub-Saharan Africa", year == 2019, causename == "Communicable, maternal, perinatal and nutritional conditions", lambda == 5, epsilon == 1) %>%
  arrange(R_bar)

cancers <- region_calculations %>%
  filter(region %in% c("China", "High-income"), year == 2019, causename == "Malignant neoplasms", lambda == 5, epsilon == 1) %>%
  arrange(R_bar)
