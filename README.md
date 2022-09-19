### Healthy Longevity Initiative

# **The economic value associated with non-communicable disease mortality: a systematic assessment by cause of death across world regions**

Stéphane Verguet^1^, Sarah Bolongaita^1^, Angela Y. Chang^2^, Diego S. Cardoso^3^, Gretchen A. Stevens^4^

^1^ Department of Global Health and Population, Harvard T.H. Chan School of Public Health, Boston, MA, USA\
^2^ Danish Institute for Advanced Study, University of Southern Denmark, Denmark\
^3^ Department of Agricultural Economics, Purdue University, USA\
^4^ Independent Researcher, Los Angeles, CA, USA

### Description

This Git Hub repository contains the analysis code for an academic paper entitled, *The economic value associated with non-communicable disease mortality: a systematic assessment by cause of death across world regions*, which was published in [ journal ] as part of the World Bank's Healthy Longevity Initiative.

The following text provides an outline of the analysis steps and guides users through the accompanying code stored in this repository.

### 1. Data cleaning

Scripts with prefix `1-` are data cleaning scripts, which primarily filter the input data to the countries, years, and causes of death that are of relevance for this analysis.

**Note:** The input data used in data cleaning scripts were pre-processed at basic, perfunctory level. The raw data was not uploaded to this repository due to size limitations; however the code to clean the raw data can be reviewed here: `init/raw-data-cleaning.R`.

#### 1-1. Country Eligibility

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

#### 1-2. Population

**Script:** `1-2_population.R`

**Objective:** Filters population data to countries and years of interest; calculates total population (i.e., not stratified by age and sex) and alpha.

**Input:**

-   `data/processed/country_info.Rda`
-   `data/input/population.csv`

**Output:**

-   `data/processed/population.Rda`

#### 1-3. GHE Recode

**Script:** `1-3_ghe-recode.R`

**Objective:** Recodes the GHE cause of death data so that causes classify as a mutually exclusive and collectively exhaustive set of causes that are of relevance for this analysis.

This script also creates two reference data frames: one containing the organizational hierarchy of recoded causes and the other containing a recode map.

**Input:**

-   `data/processed/ghe.Rda`

**Output:**

-   `data/processed/ghe_recoded.Rda`
-   `data/processed/cause_hierarchy.Rda`
-   `data/processed/cause_recode_map.Rda`
