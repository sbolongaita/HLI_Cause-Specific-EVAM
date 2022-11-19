
### 5.1 Tables



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Creating tables objects
tables <- list()



# 2 Tab 1 causes of death -------------------------------------------------

# Table name
table_name <- "Tab1.xlsx"

# Loading data
sarahLoad("cause_hierarchy", folder = "data/processed")

# Prepping table
temp1 <- cause_hierarchy %>%
  filter(level > 0) %>%
  mutate(causename = ifelse(level == 2, paste0(tab, causename), as.character(causename))) %>%
  mutate(causename = ifelse(level == 3, paste0(tab, tab, causename), as.character(causename))) %>%
  select("Level" = level, "Cause of death" = causename)

tables[[table_name]] <- temp1

write.xlsx(tables[[table_name]], file = paste("output/tables", table_name, sep = "/"))
