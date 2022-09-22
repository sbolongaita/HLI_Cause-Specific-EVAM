## Healthy Longevity Initiative

# The economic value associated with non-communicable disease mortality: a systematic assessment by cause of death across world regions

Stéphane Verguet<sup>1</sup>, Sarah Bolongaita<sup>1</sup>, Angela Y. Chang<sup>2</sup>, Diego S. Cardoso<sup>3</sup>, Gretchen A. Stevens<sup>4</sup>

<sup>1</sup> Department of Global Health and Population, Harvard T.H. Chan School of Public Health, Boston, MA, USA\
<sup>2</sup> Danish Institute for Advanced Study, University of Southern Denmark, Denmark\
<sup>3</sup> Department of Agricultural Economics, Purdue University, USA\
<sup>4</sup> Independent Researcher, Los Angeles, CA, USA

## Description

This Git Hub repository contains the analysis code for an academic paper entitled, *The economic value associated with non-communicable disease mortality: a systematic assessment by cause of death across world regions*, which was commissioned by the the World Bank's Healthy Longevity Initiative in 2022 and published in [journal] on [date].

The input data for this project can be found on [Dropbox](https://www.dropbox.com/sh/hkkbubjtbnxj070/AACpj4CQk6dyKpIJ63XVX4Ola?dl=0).

The following text provides an outline of the analysis steps and guides users through the accompanying code stored in this repository.

## 1. Data Cleaning

Scripts with prefix `1-` clean the data by filtering it to the countries, years, and causes of death that are relevant for the analysis.

**Note:** Input data for this project can be found on [Dropbox](https://www.dropbox.com/sh/hkkbubjtbnxj070/AACpj4CQk6dyKpIJ63XVX4Ola?dl=0). These input data were pre-processed at basic, perfunctory level. The raw data will not be provided, however the code to pre-process the raw data can be reviewed here: `scr/raw-data-cleaning.R`.

### 1-1. Country eligibility

**Script:** `1-1_country-eligibility.R`

**Objective:** Define and apply the eligibility criteria for inclusion in the analysis

This script defines and applies the eligibility criteria for country inclusion in the analysis. Countries were eligible for inclusion if they had populations of at least five million in 2020. Of those countries with populations of at least five million, a smaller subset were eligible for the frontier analysis: those with high-quality vital registration data and which were not excluded from the GHE 2019 analysis.

**Input:**
-   `data/input/ghe_*.csv`
-   `data/input/population.csv`
-   `data/input/quality.csv`
-   `data/input/region.csv`

**Output:**
-   `data/processed/country_info.Rda`
-   `data/processed/ghe.Rda`

### 1-2. Population

**Script:** `1-2_population.R`

**Objective:** Filter the input population data to countries of interest

This script filters the input population data to the countries of interest based on the inclusion criteria applied in `1-1_country-eligibility.R`. It then calculates the total population for countries and regions (i.e., not stratified by age and sex), as well alpha (i.e., the proportion of the total population that a given age-sex group comprises).

**Input:**
-   `data/processed/country_info.Rda`
-   `data/input/population.csv`

**Output:**
-   `data/processed/population.Rda`

### 1-3. GHE recode

**Script:** `1-3_ghe-recode.R`

**Objective:** Recode the GHE cause of death data so that causes of death are relevant for this analysis, mutually exclusive, and collectively exhaustive

This script recodes the processed GHE cause of death data (limited to analysis countries) so that causes of death are relevant for this analysis, mutually exclusive, and collectively exhaustive. This script then creates two reference data frames: one which defines the organizational hierarchy of recoded causes and the other which contains a recode map.

**Input:**
-   `data/processed/ghe.Rda`

**Output:**
-   `data/processed/ghe_recoded.Rda`
-   `data/processed/cause_hierarchy.Rda`
-   `data/processed/cause_recode_map.Rda`

## 2. Frontier Analysis

Scripts with prefix `2-` analyze and develop the age- and cause-specific mortality frontiers.

### 2-1. Frontier extraction

### `2-1_frontier-extraction.R`

****Objective:**** Extract the 10th percentile of age-cause-specific mortality (i.e., the frontier).

This script takes the recoded GHE data for frontier-eligible countries, calculates age-cause-specific mortality rates, and the extracts the frontier using a 10th percentile definition. It then harmonizes (or scales) the frontier using a level-wise approach, such that mortality from lower level causes of death sum to mortality from higher level causes of death.

****Input:****

-   `data/processed/country_info.Rda`
-   `data/processed/ghe_recoded.Rda`
-   `data/processed/population.Rda`

****Output:****

-   `data/processed/frontier_base.Rda`
-   `data/processed/frontier_harmonized.Rda`
-   `data/processed/frontier_extraction_info.Rda`

### 2-2. Frontier projection

**Script:** `2-2_frontier-projection.R`

**Objective:** Project the age-cause-specific mortality frontiers into the future (2045)

This script takes the harmonized frontier mortality rates for 2000-2019 and projects them into the future using ordinary least squares linear regression, unless there are stochastic concerns or a positive trend line from the regression, in which case an average is used.

**Input:**
-   `data/processed/country_info.Rda`
-   `data/processed/frontier_harmonized.Rda`
-   `data/processed/frontier_extraction_info.Rda`
-   `data/processed/population.Rda`

**Output:**
-   `data/processed/frontier_projected.Rda`
-   `data/processed/frotier_projection_info.Rda`

### 2-3. Frontier scaling

**Script:** `2-3_frontier_scaling.R`

**Objective:** Scale the age-cause-specific mortality frontiers with Chang et al. (2022) longevity frontiers

This script takes the harmonized and projected frontier mortality rates and scales them with the demographic longevity frontiers of an accompanying HLI paper, Chang et al. (2022), based on UN Population data. 

**Input:**
-   `data/processed/country_info.Rda`
-   `data/processed/frontier_harmonized.Rda`
-   `data/processed/frontier_extraction_info.Rda`
-   `data/processed/population.Rda`

**Output:**
-   `data/processed/chang_scaling_factors.Rda`
-   `data/processed/frontier_scaled.Rda`

## 3. Country and Region Analyses

### 3-1. Country projection

**Script:** `3-1_country-projection.R`

**Objective:** Project the country age-cause-specific mortality rates into the future (2045)

This script takes country mortality rates for 2000-2019 and projects them into the future using ordinary least squares linear regression, unless there are stochastic concerns or a positive trend line from the regression, in which case an average is used. This method is the same as the method used for the frontier projections.

**Input:**
-   `data/processed/country_info.Rda`
-   `data/processed/ghe_recoded.Rda`
-   `data/processed/population.Rda`

**Output:**
-   `data/processed/country_projected.Rda`
-   `data/processed/country_projection_info.Rda`

### 3-2. Country scaling

**Script:** `3-2_country-projection.R`

**Objective:** Scale country mortality rates with Chang et al. (2022) longevity frontiers

This script takes the projected country mortality rates and scales them with the demographic longevity frontiers of an accompanying HLI paper, Chang et al. (2022), based on UN Population data. 

**Input:**
-   `data/processed/country_info.Rda`
-   `data/processed/country_projected.Rda`
-   `data/processed/population.Rda`

**Output:**
-   `data/processed/country_scaled.Rda`

### 3-3. Region aggregation

**Script:** `3-3_region-aggregation.Rda`

**Objective:** Calculates the regional mortality rates

This script takes the scaled country projections and calculates the population-weighted average mortality rate by region.

**Input:**
-   `data/processed/country_info.Rda`
-   `data/processed/country_scaled.Rda`
-   `data/processed/population.Rda`

**Output:**
-   `data/processed/region.Rda`

## 4. Calculations

### 4-1. Country calculations

**Script:** `4-1_country-calculations.R`

**Objective:**

**Input:**
-   `data/processed/country_scaled.Rda`
-   `data/processed/frontier_scaled.Rda`
-   `data/processed/population.Rda`
-   `data/input/gni.csv`

**Output:**
-   `data/processed/country_calculations.Rda`
-   `output/data/country_calculations.Rda`

### 4-2. Region calculations

**Script:** `4-2_region-calculations.Rda`

**Objective:**

**Input:**
-   `data/processed/country_info.Rda`
-   `data/processed/frontier_scaled.Rda`
-   `data/processed/population.Rda`
-   `data/processed/region.Rda`
-   `data/input/gni.csv`

**Output:**
-   `data/processed/region_calculations.Rda`
-   `output/data/region_calculations.Rda`
