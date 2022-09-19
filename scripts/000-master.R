
### Healthy Longevity Initiative
### 000 - MASTER

# This script outlines all the order in which all scripts are to be run, and
# allows for these scripts to be called from a single place (here).


# 100 ENVIRONMENT & DATA CLEANING -----------------------------------------

# 100-level scripts either (i) set up or contribute to the analysis
# environment or (ii) conduct foundational data cleaning or data
# organization for higher level scripts.

source("01-scripts/110-ggplot2_theme.R", echo = TRUE)

source("01-scripts/120-environment.R", echo = TRUE)

source("01-scripts/130-country_eligibility.R", echo = TRUE)
# • country_info.R
# • ghe.R

source("01-scripts/140-population.R", echo = TRUE)
# • population.R
#   - Country and region populations by age, sex, and year, as well
#     as the total population and 'alpha' (the proportion of the population in
#     a given age-sex group; i.e., a population weight)

source("01-scripts/150-ghe_recode.R", echo = TRUE)
# • ghe_recoded.R
#   - GHE cause of death data that has been recoded to a set of relevant,
#     mutually exclusive and collectively exhaustive causes
# • cause_hierarchy.R
#   - Metadata on the recoded GHE causes of death, including their hierarchy
#     (information on causes' mutually exclusivity and collective exhaustion)
# • cause_recode_map.R
#   - Metadata on how the original GHE causes of death were recoded for this
#     analysis



# 200 FRONTIER ------------------------------------------------------------

# 200-level scripts identify/extract the frontier and conduct the demographic
# analysis of the frontier (i.e., projecting the frontier into the future).

source("01-scripts/210-frontier_analysis.R", echo = TRUE)
# • frontier_base.R
#   - The unharmonized frontier; the 10th percentile of age-cause-sex-specific
#     mortality rates for frontier-eligible countries
# • frontier_harmonized.Rda
#   - The harmonized frontier, in which lower level causes of death are scaled
#     so that they sum appropriately to their higher level "parent" causes of
#     death
# • frontier_analysis_info.R
#   - Metadata on the frontier, linking the unharmonized "base" frontier with the
#     harmonized, scaled frontier; this dataset shows which country age-sex groups
#     were considered at or below the frontier for a given cause in a given year

source("01-scripts/220-frontier_projection.R", echo = TRUE)
# • frontier_projected.Rda
#   - The harmonized frontier data, projected into the future (to 2045)
# • frontier_projection_info.Rda
#   - Metadata on the projected frontier, providing information on the projection
#     method and the reasoning behind the use of that projection  method

source("01-scripts/230-frontier_scaling.R", echo = TRUE)
# • 03-output_data/frontier_scaled.Rda
#   - The projected frontier data, scaled to the Chang et al. all cause frontier
#     envelope



# 300 COUNTRY -------------------------------------------------------------

# 300-level scripts deal with the development and demographic analysis of
# country-level data.

source("01-scripts/310-country_projection.R", echo = TRUE)
# • 03-output_data/country_projected.Rda
# • 03-output_data/country_projection_info.Rda

source("01-scripts/320-country_scaling.R", echo = TRUE)
# • 03-output_data/country_scaled.R


# 400 REGION --------------------------------------------------------------

# The 400-level script aggregates the mortality trajectories for the HLI
# regions.

source("01-scripts/410-region_analysis.R", echo = TRUE)
# • 03-output_data/region.R

# 500 CALCULATIONS --------------------------------------------------------

# The 500-level scripts run the calculations for the economic burden
# analysis at both the country and the regional level.

source("01-scripts/510-country_calculations.R", echo = TRUE)
# • 03-output_data/country_calculations.R

source("01-scripts/520-region_calculations.R", echo = TRUE)
# • 03-output_data/region_calculations.R


# 600 TABLES & FIGURES ----------------------------------------------------

source("01-scripts/610-tables.R", echo = TRUE)

source("01-scripts/620-figures.R", echo = TRUE)

source("01-scripts/630-appendix_tables.R", echo = TRUE)

source("01-scripts/640-appendix_figures.R", echo = TRUE)
