
### Healthy Longevity Initiative
### 610 - TABLES

# This script creates tables for the main manuscript.



# 1 ENVIRONMENT -----------------------------------------------------------

# Tidying and loading environment
sourceEnv(); tidy()
scriptName <- "610-tables"

# Creating tables objects
folder <- makeFolder(tables)
tables <- list()



# 2 Tab 1 causes of death -------------------------------------------------

# Table name
table_name <- "Tab1.xlsx"

# Loading data
sarahLoad("cause_hierarchy")

# Prepping table
temp1 <- cause_hierarchy %>%
  filter(level > 0) %>%
  mutate(causename = ifelse(level == 2, paste0(tab, causename), as.character(causename))) %>%
  mutate(causename = ifelse(level == 3, paste0(tab, tab, causename), as.character(causename))) %>%
  select("Level" = level, "Cause of death" = causename)

tables[[table_name]] <- temp1

write.xlsx(tables[[table_name]], file = paste(folder, table_name, sep = "/"))


# 3 END -------------------------------------------------------------------

# Tidying environment and notifying the end of the script
notifyScript(); tidy()
