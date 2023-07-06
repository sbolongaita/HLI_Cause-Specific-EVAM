
library(colorspace)
library(dplyr)
library(ggsci)
library(scales)
library(tidyr)

source("scr/colorFunct.R")
base::load("data/processed/cause_hierarchy.rda")

temp1 <- cause_hierarchy %>%
  select(prefix, ghecause, causename, level, starts_with("mece")) %>%
  pivot_longer(cols = starts_with("mece"), names_to = "mece.lvl") %>%
  mutate(mece.lvl = as.numeric(gsub("[a-z_.*]", "", mece.lvl))) %>%
  filter(value == TRUE) %>%
  select(-value) %>%
  arrange(prefix, mece.lvl)

temp2 <- temp1 %>%
  filter(level != 0) %>%
  mutate(lvl1 = ifelse(level == 1, causename, NA)) %>% fill(lvl1, .direction = "down") %>%
  mutate(lvl2 = ifelse(level == 2, causename, NA)) %>% group_by(lvl1) %>% fill(lvl2, .direction = "down") %>%
  mutate(lvl3 = ifelse(level == 3, causename, NA)) %>% group_by(lvl2) %>% fill(lvl3, .direction = "down") %>%
  ungroup()

# Communicable diseases - White
main <- darken("white", 0.05)
com <- temp2 %>% filter(grepl("Communicable", lvl1)) %>%
  group_by(mece.lvl) %>% arrange(mece.lvl, prefix) %>%
  mutate(row = row_number()) %>%
  mutate(color = case_when(row == 1 ~ main,
                           row == 2 ~ darken(main, 0.2),
                           TRUE ~ darken(main, 0.4)))
show_col(unique(com$color))

# Injuries - Dark grey with pattern
main <- "black"
inj <- temp2 %>% filter(grepl("Injuries", lvl1)) %>%
  group_by(mece.lvl) %>% arrange(mece.lvl, prefix) %>%
  mutate(row = row_number()) %>%
  mutate(color = case_when(row == 1 ~ main,
                           row == 2 ~ lighten(main, 0.3),
                           TRUE ~ lighten(main, 0.5)))
show_col(unique(inj$color))

# Noncommunicable disease - Rainbow
main <- ggsci::pal_material("red")(9)[6]; show_col(main)
ncd <- temp2 %>% filter(grepl("Noncommunicable", lvl1)) %>% mutate(color = NA)

ncd$color[ncd$level == 1] <- main
lvl2 <- ncd %>% filter(mece.lvl == 2) %>% pull(lvl2); lvl2 <- colorFunct(n = length(lvl2)); show_col(lvl2)
ncd$color[ncd$mece.lvl == 2] <- lvl2
ncd$color <- ifelse(is.na(ncd$color), lag(ncd$color), ncd$color)

color <- ggsci::pal_material("red")(9)[c(6, 3, 2)]; show_col(color)
ncd$color[ncd$lvl2 == "Cardiovascular diseases" & ncd$mece.lvl == 3] <- color

color <- ggsci::pal_material("light-green")(9)[c(5, 3)]; show_col(color)
ncd$color[ncd$lvl2 == "Digestive diseases" & ncd$mece.lvl == 3] <- color

color <- ggsci::pal_material("teal")(9)[8:1]; show_col(color)
ncd$color[ncd$lvl2 == "Malignant neoplasms" & ncd$mece.lvl == 3] <- color

color <- ggsci::pal_material("light-blue")(9)[c(7, 5)]; show_col(color)
ncd$color[ncd$lvl2 == "Respiratory diseases" & ncd$mece.lvl == 3] <- color

show_col(ncd$color[ncd$mece.lvl == 1])
show_col(ncd$color[ncd$mece.lvl == 2])
show_col(ncd$color[ncd$mece.lvl == 3])

# Combining
cause_aesth <- bind_rows(com, ncd, inj) %>%
  mutate(mece.lvl = as.factor(mece.lvl)) %>%
  arrange(mece.lvl, prefix) %>%
  select(-c(prefix, level, row, starts_with("lvl"))) %>%
  ungroup()

# __+ cause_aesth ---------------------------------------------------------
save(cause_aesth, file ="data/processed/cause_aesth.rda")
