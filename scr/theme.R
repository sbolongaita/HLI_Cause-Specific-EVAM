library(extrafont)
library(ggplot2)
library(ggthemes)
library(remotes)
library(sysfonts)
library(utils)

# The `extrafont` package was buggy at the time I wrote this script; an older
# version of the `Rttf2pt1` package is necessary.
# if(utils::packageVersion("Rttf2pt1") != "1.3.8"){
#   utils::remove.packages("Rttf2pt1")
#   remotes::install_version("Rttf2pt1", version = "1.3.8")
# }

# The font included in this `ggplot` theme will not work if you do not have it
# installed on your computer. It is a free Google font available here: https://fonts.google.com/specimen/Barlow
# if(!"Barlow" %in% extrafont::fonts()){
#   sysfonts::font_add_google("Barlow")
#   extrafont::font_import()
#   extrafont::loadfonts(device = "pdf", quiet = TRUE)
# }

# Defining the ggplot theme
theme <- theme(
  # Base settings
  rect = element_rect(fill = NA, color = NA),
  text = element_text(family = "Barlow", face = "plain", size = 11),
  # Plot settings
  plot.background = element_rect(fill = NA, color = NA),
  plot.margin = margin(10, 10, 10, 10),
  plot.title = element_text(face = "bold", size = 18,  hjust = 0, vjust = 0, margin = margin(0, 0, 10, 15)),
  plot.subtitle = element_text(size = 14, hjust = 0, vjust = 0, margin = margin(5, 0, 10, 0)),
  plot.caption = element_text(size = 11, margin = margin(10, 0, 0, 0), hjust = 0),
  # Panel settings
  panel.background = element_rect(fill = "white", color = NA),
  panel.grid.major = element_line(color = alpha("black", 0.2), linewidth = 0.2),
  panel.grid.minor = element_blank(),
  # panel.grid.minor.y = element_line(color = alpha("black", 0.2), linewidth = 0.2),
  panel.spacing = unit(10, units = "pt"),
  # Axis settings
  axis.title.x = element_text(size = 12, margin = margin(10, 0, 0, 0)),
  axis.title.y = element_text(size = 12, margin = margin(0, 10, 0, 0), angle = 90),
  axis.text = element_text(),
  axis.line = element_line(color = alpha("black", 0.8), linewidth = 0.2),
  axis.ticks = element_line(color = alpha("black", 0.2), linewidth = 0.2),
  # Legend settings
  legend.title = element_text(size = 12),
  legend.text = element_text(),
  legend.text.align = 0,
  legend.margin = margin(5, 5, 5, 5),
  legend.background = element_rect(),
  legend.key = element_blank(),
  legend.key.size = unit(20, "pt"),
  # Strip text settings (facet plots)
  strip.background = element_rect(fill = alpha("black", 0.1)),
  strip.placement = "outside",
  strip.text = element_text(size = 12, hjust = 0.5, vjust = 0.5, margin = margin(5, 5, 5, 5))
)

# Setting the ggplot theme
theme_set(theme)
