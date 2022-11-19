
### 1.3 GHE Recode

# This script recodes the processed GHE cause of death data (limited to
# analysis countries) so that causes of death are relevant for this analysis,
# mutually exclusive, and collectively exhaustive. This script then creates
# two reference data frames: one which defines the organizational hierarchy
# of the recoded causes and the another which maps the cause recode between
# the original GHE causes and the ones in this analysis.



# 1 Loading data ----------------------------------------------------------

# Applying the standard project environment
applyEnv()

# Loading data
sarahLoad("ghe", folder = "data/processed")



# 2 Recoding causes -------------------------------------------------------

# Creating new ghecause and causename variables
temp1 <- ghe %>%
  mutate(ghecause2 = ghecause, causename2 = causename)


# * 2.1 L1: Communicable, maternal, perinatal,and nutritional cond. -------

# Removing level 2:4 causes that are under communicable, maternal, perinatal
# and nutritional conditions causes
temp2 <- temp1 %>%
  filter(ghecause2 %notin% 20:590)


# * 2.2 L1: Noncommunicable diseases --------------------------------------

temp3 <- temp2 %>%
  # Removing level 3 and 4 causes that are under level 2 causes we're not interested in
  filter(ghecause2 %notin% c(811:814, 950:1010, 1030:1090, 1270:1320,
                             1331:1339, 1350:1390, 1410:1460, 1480:1502)) %>%
  # Re-coding level 2 causes that aren't of interest
  mutate(ghecause2 = ifelse(ghecause2 %in% c(790, 810, 940, 1020, 1260, 1330,
                                             1340, 1400, 1470, 1505), 600600, ghecause2)) %>%
  mutate(causename2 = ifelse(ghecause2 == 600600, "Other noncommunicable diseases", causename2))

# ** 2.2(a) L2: Malignant neoplasms ---------------------------------------
temp4 <- temp3 %>%
  # Removing level 4 malignant neoplasm causes
  filter(ghecause2 %notin% c(621:623, 661:664, 691:692, 761:763)) %>%
  # Re-coding level 3 malignant neoplasm causes that aren't of interest
  mutate(ghecause2 = ifelse(ghecause2 %in% c(650, 670, 690, 720:780), 610610, ghecause2)) %>%
  mutate(causename2 = ifelse(ghecause2 == 610610, "Other malignant neoplasms", causename2))

# ** 2.2(b) L2: Mental and substance use disorders ------------------------
temp5 <- temp4 %>%
  # Removing level 4 mental and substance use disorder causes
  filter(ghecause2 %notin% c(831:832, 871:875, 911:912)) %>%
  # Re-coding level 3 mental and substance use disorder causes that aren't of interest
  mutate(ghecause2 = ifelse(ghecause2 %in% c(830:850, 870:930), 820820, ghecause2)) %>%
  mutate(causename2 = ifelse(ghecause2 == 820820, "Other mental and substance use disorders",
                             causename2))

# ** 2.2(c) L2: Cardiovascular diseases -----------------------------------
temp6 <- temp5 %>%
  # Removing level 4 cardiovascular diseases causes
  filter(ghecause2 %notin% c(1141, 1142)) %>%
  # Re-coding level 3 cardiovascular diseases causes that aren't of interest
  mutate(ghecause2 = ifelse(ghecause2 %in% c(1110:1120, 1150:1160), 11001100, ghecause2)) %>%
  mutate(causename2 = ifelse(ghecause2 == 11001100, "Other cardiovascular diseases",
                             causename2))

# ** 2.2(d) L2: Respiratory diseases --------------------------------------
temp7 <- temp6 %>%
  # Re-coding level 3 respiratory diseases causes that aren't of interest
  mutate(ghecause2 = ifelse(ghecause2 %in% c(1190:1200), 11701170, ghecause2)) %>%
  mutate(causename2 = ifelse(ghecause2 == 11701170, "Other respiratory diseases",
                             causename2))

# ** 2.2(e) L2: Digestive diseases ----------------------------------------
temp8 <- temp7 %>%
  # Removing level 4 digestive disease causes
  filter(ghecause2 %notin% c(1231:1234)) %>%
  # Re-coding level 3 digestive diseases causes that aren't of interest
  mutate(ghecause2 = ifelse(ghecause2 %in% c(1220, 1240:1250), 12101210, ghecause2)) %>%
  mutate(causename2 = ifelse(ghecause2 == 12101210, "Other digestive diseases", causename2))



# * 2.3 L1: Injuries ------------------------------------------------------

temp9 <- temp8 %>%
  # Removing level 3 causes that are under level 2 causes that aren't of interest
  filter(ghecause2 %notin% c(1610:1630))

# ** 2.3(a) L2: Unintentional injuries ------------------------------------
temp10 <- temp9 %>%
  # Re-coding level 3 unintentional injuries causes that aren't of interest
  mutate(ghecause2 = ifelse(ghecause2 %in% c(1540:1590), 15201520, ghecause2)) %>%
  mutate(causename2 = ifelse(ghecause2 == 15201520, "Other unintentional injuries", causename2))



# 3 Summing recoded causes ------------------------------------------------

ghe_recoded <- temp10 %>%
  group_by(iso3, year, sex, age, ghecause2, causename2) %>%
  dplyr::summarize(dths = round(sum(dths)), .groups = "drop") %>%
  dplyr::select(iso3, year, sex, age, ghecause = ghecause2, causename = causename2, dths)



# 4 Checking recode -------------------------------------------------------

check <- left_join(ghe_recoded, ghe,
                   by = c("iso3", "year", "sex", "age", "ghecause", "causename")) %>%
  filter(round(dths.x) != round(dths.y))

if(nrow(check) > 0){
  warning("Recode may be incorrect")
  check
}

# __+ ghe_recoded ---------------------------------------------------------
sarahSave("ghe_recoded", folder = "data/processed")



# 5 Creating cause recode map ---------------------------------------------

cause_recode_map <- temp10 %>%
  select(original_ghecause = ghecause, original_causename = causename,
         recoded_ghecause = ghecause2, recoded_causename = causename2) %>%
  unique() %>%
  dplyr::select(starts_with("original"), starts_with("recoded"))

# __+ cause_recode_map ----------------------------------------------------
sarahSave("cause_recode_map", folder = "data/processed")
sarahSave("cause_recode_map", folder = "output/data")


# 6 Creating cause hierarchy dataframe ------------------------------------

temp1 <- ghe_recoded %>%
  dplyr::select(ghecause, causename) %>%
  unique()

# Adding level and parents causes
temp2 <- unique(temp1) %>%
  mutate(level = case_when(ghecause == 0 ~ 0,
                           ghecause %in% c(10, 600, 1510) ~ 1,
                           ghecause %in% c(610, 800, 820, 1100, 1170, 1210,
                                           600600, 1520, 1600) ~ 2,
                           TRUE ~ 3),
         parent_ghecause = case_when(ghecause %in% c(10, 600, 1510) ~ 0,
                                     ghecause %in% c(610, 800, 820, 1100, 1170,
                                                     1210, 600600) ~ 600,
                                     ghecause %in% c(1130, 1140, 11001100) ~ 1100,
                                     ghecause %in% c(1230, 12101210) ~ 1210,
                                     ghecause %in% c(620, 630, 640, 660, 680, 700,
                                                     710, 610610) ~ 610,
                                     ghecause %in% c(860, 820820) ~ 820,
                                     ghecause %in% c(1180, 11701170) ~ 1170,
                                     ghecause %in% c(1520, 1600) ~ 1510,
                                     ghecause %in% c(1530, 15201520) ~ 1520),
         parent_causename = case_when(ghecause %in% c(10, 600, 1510) ~ "All causes",
                                      ghecause %in% c(610, 800, 820, 1100, 1170, 1210,
                                                      600600) ~ "Noncommunicable diseases",
                                      ghecause %in% c(1130, 1140, 11001100) ~ "Cardiovascular diseases",
                                      ghecause %in% c(1230, 12101210) ~ "Digestive diseases",
                                      ghecause %in% c(620, 630, 640, 660, 680, 700,
                                                      710, 610610) ~ "Malignant neoplasms",
                                      ghecause %in% c(860, 820820) ~ "Mental and substance use disorders",
                                      ghecause %in% c(1180, 11701170) ~ "Respiratory diseases",
                                      ghecause %in% c(1520, 1600) ~ "Injuries",
                                      ghecause %in% c(1530, 15201520) ~ "Unintentional injuries"))

# Adding prefixes (for figure file names and sorting)
temp3 <- temp2 %>%
  mutate(prefix = case_when(causename == "All causes" ~ "000",
                            causename == "Communicable, maternal, perinatal and nutritional conditions" ~ "100",
                            causename == "Noncommunicable diseases" ~ "200",
                            causename == "Cardiovascular diseases" ~ "210",
                            causename == "Ischaemic heart disease" ~ "211",
                            causename == "Stroke" ~ "212",
                            causename == "Other cardiovascular diseases" ~ "213",
                            causename == "Diabetes mellitus" ~ "220",
                            causename == "Digestive diseases" ~ "230",
                            causename == "Cirrhosis of the liver" ~ "231",
                            causename == "Other digestive diseases" ~ "232",
                            causename == "Malignant neoplasms" ~ "240",
                            causename == "Breast cancer" ~ "241",
                            causename == "Cervix uteri cancer" ~ "242",
                            causename == "Liver cancer" ~ "243",
                            causename == "Mouth and oropharynx cancers" ~ "244",
                            causename == "Oesophagus cancer" ~ "245",
                            causename == "Stomach cancer" ~ "246",
                            causename == "Trachea, bronchus, lung cancers" ~ "247",
                            causename == "Other malignant neoplasms" ~ "248",
                            causename == "Mental and substance use disorders" ~ "250",
                            causename == "Alcohol use disorders" ~ "251",
                            causename == "Other mental and substance use disorders" ~ "252",
                            causename == "Respiratory diseases" ~ "260",
                            causename == "Chronic obstructive pulmonary disease" ~ "261",
                            causename == "Other respiratory diseases" ~ "262",
                            causename == "Other noncommunicable diseases" ~ "270",
                            causename == "Injuries" ~ "300",
                            causename == "Intentional injuries" ~ "310",
                            causename == "Unintentional injuries" ~ "320",
                            causename == "Road injury" ~ "321",
                            causename == "Other unintentional injuries" ~ "322"))

# Adding main
temp4 <- temp3 %>%
  mutate(main_ghecause = case_when(grepl("^1", prefix) ~ 10,
                                   grepl("^2", prefix) ~ 600,
                                   grepl("^3", prefix) ~ 1510),
         main_causename = case_when(grepl("^1", prefix) ~ "Communicable, maternal, perinatal and nutritional conditions",
                                    grepl("^2", prefix) ~ "Noncommunicable diseases",
                                    grepl("^3", prefix) ~ "Injuries"))

# Adding mutually exclusive collectively exhaustive filters
temp5 <- temp4 %>%
  mutate(causename = factor(causename, levels = causename.levels),
         childless = ghecause %notin% parent_ghecause) %>%
  mutate(mece_lvl1 = level == 1 | (level < 1 & childless == TRUE),
         mece_lvl2 = level == 2 | (level < 2 & childless == TRUE),
         mece_lvl3 = level == 3 | (level < 3 & childless == TRUE))

cause_hierarchy <- temp5 %>%
  dplyr::select(prefix, ghecause, causename, level, starts_with("main"),
                starts_with("parent"), childless, starts_with("mece")) %>%
  arrange(prefix)

# __+ cause_hierarchy -----------------------------------------------------
sarahSave("cause_hierarchy", folder = "data/processed")
