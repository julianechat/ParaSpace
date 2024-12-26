# ParaSpace

**Author :** Juliane Vigneault

**Contact information :** [juliane.vigneault\@umontreal.ca](mailto:juliane.vigneault@umontreal.ca){.email}

**Date :** 2024-02-26

## Overview

This R project results of a field study that aimed to explore infection prevalence in fish communities of lakes. Analyses focus on three spatial scales (landscape, lake and site) and the comparison of various sampling methods (minnow trap, seine net and snorkeling transect).

## Requirements

`Scripts` files require R & RStudio to run properly.

-   [R Download](https://cran.r-project.org/)

-   [RStudio Download](https://posit.co/downloads/)

Installation of the following `packages` are also required (all available on CRAN): dplyr, vegan, tidyr, ggplot2, gratia, cowplot, patchwork, colorspace, gt, janitor, splitstackshape, stringr, writexl, tibble, measurements, mgcv, itsadug, broom, sf , dunn.test and ggspatial.

## Roadmap

`Script` files should be run in the following order :

1.  `DataTransformation.R` : Data formatting and transformation for analyses. Fishing (seine net and minnow trap) and Transect are formatted in both long and large format. Combined data set is created for all methods.

2.  `InfectionPrevalence.R` : Calculation of prevalence estimates for each sampling method (Combined, Minnow trap, Seine net and Transect) and spatial scale (Landscape, Lake and Site). Prevalence estimates are presented at fish community level and species level.

3.  `CommunityMetrics.R` : Calculation of community metrics. Species richness, Gini-Simpson diversity and Evenness is calculated for each sampling method (Combined, Minnow trap, Seine net and Transect) and spatial scale (Landscape, Lake and Site). \*Only transect method metrics are calculated at site-scale.

4.  `Infection_Length.R` : Relationship exploration between fish length and infection metrics.

5.  `AccumulationCurves.R` : Landscape scale analyses - Accumulation curves through an increasing sampling effort gradient. Species accumulation curves, Infected individuals accumulation curves, Total individuals accumulation curves and Prevalence accumulation curves are presented for each method (Combined, Minnow trap, Seine net and Transect). Statistical comparison between methods are given for the prevalence accumulation curves. Summary figures are also presented.

6.  `FrequencyDistributions.R` Lake scale analyses - Frequency distributions of lake fish community prevalence estimates according to the different sampling methods (Combined, Minnow trap, Seine net and Transect). Frequency distribution of small and large minnow traps are also given. Summary figures are presented.

7.  `Prevalence_Maps.R` Prevalence maps. Bubble maps of site-scale Lepomis gibbosus prevalence are presented for each lake. Lake-scale fish community prevalence maps are presented for each method (Combined, Minnow trap, Seine net and Transect). Watershed limits representation are in option. Method comparison summary figure of lake prevalence maps is presented along with corresponding frequency distribution plot.

8.  `ModelAnalysis_DataFrame.R` Building data frame for model analyses.

9.  `ModelAnalysis_DataExploration.R` Data exploration for model analyses. Includes variables outliers, collinearity between explanarory variables and relationships between response and explanatory variables for transect-scale and lake-scale environmental variables.

10. `ModelAnalysis_GAMs.R` Site-scale analyses. Generalized additive mixed models exploring relationships between transect-scale prevalence estimates and environmental variables. Include models visualization and validation. Summary figure and model estimates are given.

11. `SupportInformation_Tables.R` Creation of support information tables that are not included in other scripts.

## Acknowledgements

I acknowledge conception support and code revision by Éric Harvey (Université du Québec à Trois-Rivières).
