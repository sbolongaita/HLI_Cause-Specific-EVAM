base::load("data/processed/cause_hierarchy.Rda")

graphing.groups <- list("Level" = list("Level 0" = cause_hierarchy$ghecause[cause_hierarchy$level == 0],
                                       "Level 1" = cause_hierarchy$ghecause[cause_hierarchy$level == 1],
                                       "Level 2" = cause_hierarchy$ghecause[cause_hierarchy$level == 2],
                                       "Level 3" = cause_hierarchy$ghecause[cause_hierarchy$level == 3]),
                        "MECE" = list("Level 1" = cause_hierarchy$ghecause[cause_hierarchy$mece_lvl1],
                                      "Level 2" = cause_hierarchy$ghecause[cause_hierarchy$mece_lvl2],
                                      "Level 3" = cause_hierarchy$ghecause[cause_hierarchy$mece_lvl3]),
                        "Main" = list("Communicable, maternal, perinatal and nutritional conditions" = cause_hierarchy$ghecause[cause_hierarchy$main_causename == "Communicable, maternal, perinatal and nutritional conditions"],
                                      "Noncommunicable diseases" = cause_hierarchy$ghecause[cause_hierarchy$main_causename == "Noncommunicable diseases"],
                                      "Injuries" = cause_hierarchy$ghecause[cause_hierarchy$main_causename == "Injuries"]))

# Causename levels
causename.levels <- as.character(cause_hierarchy$causename)

# Sex-specific causenames
sex.specific <- c("Breast cancer", "Cervix uteri cancer")

# Log breaks and labels
log.breaks <- 10^seq(-5, 5)
log.labels <- c(expression(10^-5), expression(10^-4),
                "0.001", "0.01", "0.1", "1", "10", "100", "1K", "10K", "100K")

# Table helpers
new.line <- "~NEWLINE~"
tab <- "~TAB~"

# Table symbols - Asterisk, dagger, double dagger, section sign, double
# asterisk, double double dagger
symbols <- c("\u002A", "\u2020", "\u2021", "\u00A7", "\u002A\u002A", "\u2021\u2021")

# Figure panel ids
ids <- paste0("(", c(LETTERS, paste0("A", LETTERS)), ") ")

# Figure captions
captions <- list("Zero" = "* Values of zero not depicted on the log scale",
                 "R bar" = bquote("Note: The displayed"~bar(R)~"estimates are calculated using an income elasticity of 1."))
