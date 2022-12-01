
library(forcats)
library(dplyr)
library(data.tree)
library(tidyr)
library(openxlsx)

# This script recodes the GHE causes of death so that causes of death are
# relevant for this analysis, mutually exclusive, and collectively exhaustive.
# The document `doc/GHE-2019_cause-categories.pdf` contains information on
# the GHE 2019 cause of death categories.

base::load("data/processed/ghe.Rda")

# Original causes
original <- ghe %>% select(ghecause, causename) %>% unique()

# Adding cause levels (from GHE 2019 hierarchy)
temp1 <- original %>%
  mutate(level = case_when(ghecause == 0 ~ 0,
                           ghecause %in% c(10, 600, 1510) ~ 1,
                           ghecause %in% c(20, 380, 420, 490, 540,
                                           610, 790, 800, 810, 820, 940, 1020, 1100, 1170, 1210, 1260, 1330, 1340, 1400, 1470, 1505,
                                           1520, 1600) ~ 2,
                           ghecause %in% c(50:90, 101:102, 130:160, 186:205, 220:320, 340:362, 621:623, 661:664, 691:692, 761:763,
                                           831:832, 871:875, 911:912, 1141:1142, 1231:1234, 1271:1273) ~ 4,
                           TRUE ~ 3)) %>%
  select(level, everything())

# Original cause tree -----------------------------------------------------

original_cause <- temp1 %>%
  mutate(level.0 = ifelse(level == 0, causename, NA)) %>% fill(level.0, .direction = "down") %>%
  group_by(level.0) %>% mutate(level.1 = ifelse(level == 1, causename, NA)) %>% fill(level.1, .direction = "down") %>%
  group_by(level.1) %>% mutate(level.2 = ifelse(level == 2, causename, NA)) %>% fill(level.2, .direction = "down") %>%
  group_by(level.2) %>% mutate(level.3 = ifelse(level == 3, causename, NA)) %>% fill(level.3, .direction = "down") %>%
  group_by(level.3) %>% mutate(level.4 = ifelse(level == 4, causename, NA)) %>%
  unite("pathString", c(level.0, level.1, level.2, level.3, level.4), sep = "/", na.rm = TRUE)
original_cause_tree <- as.Node(original_cause)

# __ + original_cause_tree ------------------------------------------------
save(original_cause_tree, file ="data/processed/original_cause_tree.rda")

# Omitting lower level causes that aren't of interest
temp2 <- temp1 %>%
  mutate(omit = case_when(level == 4 ~ TRUE,
                          ghecause < 600 & level > 2 ~ TRUE,
                          ghecause > 810 & ghecause < 1110 & level > 2 ~ TRUE,
                          ghecause > 1170 & ghecause < 1210 & level > 2 ~ TRUE,
                          ghecause > 1260 & ghecause < 1505 & level > 2 ~ TRUE,
                          ghecause > 1600 & level > 2 ~ TRUE,
                          TRUE ~ FALSE)) %>%
  mutate(ghecause2 = ifelse(omit, NA, ghecause))

# Recoding causes
temp3 <- temp2 %>%
  mutate(ghecause2 = case_when(is.na(ghecause2) ~ NA_real_,
                               ghecause2 == 380 ~ 20, # Infectious and parasitic diseases
                               ghecause2 == 490 ~ 420, # Maternal and neonatal conditions
                               ghecause2 %in% c(650, 670, 690, 720:770) ~ 780, # Other malignant neoplasms
                               ghecause2 %in% c(1110, 1120, 1150) ~ 1160, # Other cardiovascular diseases
                               ghecause2 %in% c(1220, 1240:1250) ~ 1250, # Other digestive diseases
                               ghecause2 %in% c(1540:1590) ~ 1590, # Other unintentional injuries
                               ghecause2 %in% c(790, 810, 820, 940, 1020, 1260,
                                                1330, 1340, 1400, 1470, 1505) ~ 600600, # Other noncommunicable diseases,
                               TRUE ~ as.numeric(ghecause2))) %>%
  mutate(causename2 = case_when(is.na(ghecause2) ~ NA_character_,
                                ghecause2 == 20 ~ "Infectious and parasitic diseases", # Infectious and parasitic diseases
                                ghecause2 == 420 ~ "Maternal and neonatal conditions", # Maternal and neonatal conditions
                                ghecause2 == 780 ~ "Other malignant neoplasms", # Other malignant neoplasms
                                ghecause2 == 1160 ~ "Other cardiovascular diseases", # Other cardiovascular diseases
                                ghecause2 == 1250 ~ "Other digestive diseases", # Other digestive diseases
                                ghecause2 == 1590 ~ "Other unintentional injuries", # Other unintentional injuries
                                ghecause2 == 600600 ~ "Other noncommunicable diseases", # Other noncommunicable diseases,
                             TRUE ~ as.character(causename)))

# Cause recode map --------------------------------------------------------
cause_recode_map <- temp3 %>%
  select(original_ghecause = ghecause, original_causename = causename,
         recoded_ghecause = ghecause2, recoded_causename = causename2)

# __ + cause_recode_map ---------------------------------------------------
save(cause_recode_map, file ="data/processed/cause_recode_map.rda")


# Recoded cause tree ------------------------------------------------------
recoded_cause <- temp3 %>%
  mutate(level.0 = ifelse(level == 0, causename, NA)) %>%
  fill(level.0, .direction = "down") %>%
  group_by(level.0) %>%
  mutate(level.1 = ifelse(level == 1, causename, NA)) %>%
  fill(level.1, .direction = "down") %>%
  group_by(level.1) %>%
  mutate(level.2 = ifelse(level == 2, causename, NA)) %>%
  fill(level.2, .direction = "down") %>%
  group_by(level.2) %>%
  mutate(level.3 = ifelse(level == 3, causename, NA)) %>%
  fill(level.3, .direction = "down") %>%
  group_by(level.3) %>%
  mutate(level.4 = ifelse(level == 4, causename, NA)) %>%
  unite("pathString", c(level.0, level.1, level.2, level.3, level.4), sep = "/", na.rm = TRUE)

original_to_recoded_cause_tree <- ToDataFrameTree(as.Node(recoded_cause), "causename2") %>%
  rename(original = levelName, recoded = causename2)

# __ + original_to_recoded_cause_tree ---------------------------------------
save(original_to_recoded_cause_tree, file ="data/processed/original_to_recoded_cause_tree.rda")
write.xlsx(original_to_recoded_cause_tree, file = "output/data/original_to_recoded_cause_tree.xlsx")


# Cause hierarchy ---------------------------------------------------------
temp4 <- temp3 %>%
  filter(!omit) %>%
  select(ghecause = ghecause2, causename = causename2, level) %>%
  unique() %>%
  mutate(level.0 = ifelse(level == 0, causename, NA)) %>% fill(level.0, .direction = "down") %>%
  group_by(level.0) %>% mutate(level.1 = ifelse(level == 1, causename, NA)) %>% fill(level.1, .direction = "down") %>%
  group_by(level.1) %>% mutate(level.2 = ifelse(level == 2, causename, NA)) %>% fill(level.2, .direction = "down") %>%
  group_by(level.2) %>% mutate(level.3 = ifelse(level == 3, causename, NA)) %>%
  arrange(!is.na(level.1), level.1 == "Injuries", level.1,
          !is.na(level.2), grepl("Other", level.2), level.2,
          !is.na(level.3), grepl("Other", level.3), level.3)

other <- function(x){
  y <- unique(c(sort(x[!grepl("Other", x)]), x[grepl("Other", x)]))
  return(y)
}

temp5 <- temp4 %>% ungroup() %>%
  mutate(level.1_prefix = as.numeric(fct_relevel(as.factor(level.1), "Injuries", after = Inf)) * 100) %>%
  group_by(level.1) %>%
  mutate(level.2_prefix = as.numeric(fct_relevel(as.factor(level.2), other)) * 10) %>%
  group_by(level.2) %>%
  mutate(level.3_prefix = as.numeric(fct_relevel(as.factor(level.3), other))) %>%
  ungroup() %>%
  mutate_at(vars(level.1_prefix, level.2_prefix, level.3_prefix), ~ ifelse(is.na(.), 0, .)) %>%
  mutate(prefix = level.1_prefix + level.2_prefix + level.3_prefix) %>%
  mutate(parent_causename = case_when(level == 0 ~ NA_character_,
                            causename == level.1 ~ "All causes",
                            causename == level.2 ~ level.1,
                            TRUE ~ level.2)) %>%
  select(prefix, ghecause, causename, level, main_causename = level.1, parent_causename) %>%
  arrange(prefix)

# Adding ghecause codes for main_causename and parent
ghecause <- setNames(temp5$ghecause, temp5$causename)
lookup <- function(x){
  unname(ghecause[x])
}

temp6 <- temp5 %>%
  mutate(main_ghecause = lookup(main_causename),
         parent_ghecause = lookup(parent_causename)) %>%
  select(prefix, ghecause, causename, level, main_ghecause, main_causename, parent_ghecause, parent_causename)

# Adding mutually exclusive collectively exhaustive filters
cause_hierarchy <- temp6 %>%
  mutate(childless = !(ghecause %in% parent_ghecause)) %>%
  mutate(mece_lvl1 = level == 1 | (level < 1 & childless == TRUE),
         mece_lvl2 = level == 2 | (level < 2 & childless == TRUE),
         mece_lvl3 = level == 3 | (level < 3 & childless == TRUE))

# __ + cause_hierarchy ----------------------------------------------------
save(cause_hierarchy, file ="data/processed/cause_hierarchy.rda")
