## Healthy Longevity Initiative

# The economic value associated with non-communicable disease mortality: a systematic assessment by cause of death across world regions

Stéphane Verguet<sup>1</sup>, Sarah Bolongaita<sup>1</sup>, Angela Y. Chang<sup>2</sup>, Diego S. Cardoso<sup>3</sup>, Gretchen A. Stevens<sup>4</sup>

<sup>1</sup> Department of Global Health and Population, Harvard T.H. Chan School of Public Health, Boston, MA, USA\
<sup>2</sup> Danish Institute for Advanced Study, University of Southern Denmark, Denmark\
<sup>3</sup> Department of Agricultural Economics, Purdue University, USA\
<sup>4</sup> Independent Researcher, Los Angeles, CA, USA

## Description

This Git Hub repository contains the analysis code for an academic paper entitled, *The economic value associated with non-communicable disease mortality: a systematic assessment by cause of death across world regions*, which was published in [ journal ] as part of the World Bank's Healthy Longevity Initiative.

The following text provides an outline of the analysis steps and guides users through the accompanying code stored in this repository.

## 1. Data Cleaning

Scripts with prefix `1-` are data cleaning scripts, which primarily filter the input data to the countries, years, and causes of death that are of relevance for this analysis.

**Note:** The input data used in data cleaning scripts were pre-processed at basic, perfunctory level. The raw data was not uploaded to this repository due to size limitations; however the code to clean the raw data can be reviewed here: `init/raw-data-cleaning.R`.

### 1-1. Country eligibility

**Script:** `1-1_country-eligibility.R`

**Objective:** Defines and applies the eligibility criteria for inclusion in the analysis.

Countries were eligible for inclusion in the analysis if they had populations of at least five million in 2020. Of those countries with populations of at least five million, a smaller subset were eligible for the frontier analysis if they had (a) high-quality vital registration data and (b) were not excluded from the GHE 2019 analysis.

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

**Objective:** Filters population data to countries and years of interest; calculates total population (i.e., not stratified by age and sex) and alpha.

**Input:**

-   `data/processed/country_info.Rda`
-   `data/input/population.csv`

**Output:**

-   `data/processed/population.Rda`

### 1-3. GHE recode

**Script:** `1-3_ghe-recode.R`

**Objective:** Recodes the GHE cause of death data so that causes classify as a mutually exclusive and collectively exhaustive set of causes that are of relevance for this analysis.

This script also creates two reference data frames: one containing the organizational hierarchy of recoded causes and the other containing a recode map.

**Input:**

-   `data/processed/ghe.Rda`

**Output:**

-   `data/processed/ghe_recoded.Rda`
-   `data/processed/cause_hierarchy.Rda`
-   `data/processed/cause_recode_map.Rda`

## 2. Frontier Analysis

Scripts with prefix `2-` are those analyze and develop the mortality frontiers.

### 2-1. Frontier extraction

****Script:**** `2-1_frontier-extraction.R`

****Objective:**** Extract the 10th percentile of age-cause-specific mortality.

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

**Objective:**

**Input:**
-   `data/processed/country_info.R/`
-   `data/processed/frontier_harmonized.R/`
-   `data/processed/frontier_extraction_info.R/`
-   `data/processed/population.R/`

**Output:**

### 2-3. Frontier scaling

**Script:**

**Objective:**

**Input:**

**Output:**

## 3. Country and Region Analyses

### 3-1. Country projection

**Script:**

**Objective:**

**Input:**

**Output:**

### 3-2. Country scaling

**Script:**

**Objective:**

**Input:**

**Output:**

### 3-3. Region aggregation

**Script:**

**Objective:**

**Input:**

**Output:**

## 4. Calculations

### 5-1. Country calculations

**Script:**

**Objective:**

**Input:**

**Output:**

### 5-2. Region calculations

**Script:**

**Objective:**

**Input:**

**Output:**
