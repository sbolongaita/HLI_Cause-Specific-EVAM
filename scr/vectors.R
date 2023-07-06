base::load("data/processed/cause_hierarchy.Rda")

# Sex-specific causenames
sex.specific <- c("Breast cancer", "Cervix uteri cancer")

# Focus causenames
focus <- c("All causes", "Communicable, maternal, perinatal and nutritional conditions",
           "Cardiovascular diseases", "Malignant neoplasms", "Injuries")

# Causename levels
causename.levels <- as.character(cause_hierarchy$causename)
causename.levels2 <- ifelse(causename.levels %in% sex.specific, paste(causename.levels, "(females)"), causename.levels)

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

# Covid years
covid <- c(2020, 2021, 2022, 2023)
