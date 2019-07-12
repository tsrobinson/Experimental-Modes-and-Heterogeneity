# Multi-Modes for Detecting Experimental Measurement Error

Raymond Duch, Denise Laroze, Thomas Robinson and Pablo Beramendi

Last edit: July 2019

## Replication Guide

*Replication files:*
1. main_replication.R
2. simulation_replication.R
3. indian_vignette_replication.R

The majority of the replication code for this paper is found in main_replication.R. Code for the simulation in Section 2 (Figure 1) and the vignette experiment in Section 5 (Table 3) are stored in separate replication scripts.

The remainder of this guide walks through each section of the main replication file, to aid reproduction of the paper's results.

*All code should be run sequentially as it appears in main_replication.R.*

### NB: Working directory

The working directory should be set to the main directory of this replication package. All code and data is accessed via relative-paths.

## 0. Prerequisites

Includes package initialisation and initial seed setting commands. The results in the code were run using the following package versions (latest releases as of 27 June 2019) using R version 3.6.0.:

foreign_0.8-71
broom_0.5.2
ggpubr_0.2.1
stargazer_5.2.2
CBPS_0.20
scales_1.0.0
gridExtra_2.3
effects_4.1-1
plyr_1.8.4
plm_2.0-2
lmtest_0.9-37
xtable_1.8-4
FindIt_1.1.4
BayesTree_0.3-1.4
clusterSEs_2.6.1
lme4_1.1-21
sjstats_0.17.5
dplyr_0.8.1
ltm_1.1-1
wesanderson_0.3.6
stringr_1.4.0
tidyverse_1.2.1

## 1. Functions

Functions required to execute aspects of the code:0o

- *consist.risk* calculates the consistency of participants risk attitudes in experiment
- *redist.fun* normalizes distribution of abilities
- *bracket* adds brackets around a vector of numbers, used in table production for reporting standard errors.
- *abilityCalc* calculates the relative ability of participants within the lying experiments
- *icc_modes* calculates the intra-class correlation (ICC) coefficient for the lying experiment (report rate and RET performance)
- *icc_se* bootstraps a standard error corresponding to icc_modes ICC estimate

## 2. Necessary data cleaning

All original data files are included. This section runs the necessary data formatting and cleaning required to run the main analysis, generating mode-specific dataframes (*baseline.uk*, *lab.online.sync*, *cess.online.panel*, and *mturk.ds*), and a combined dataframe of all four modes (*p.data*) used for the machine-learning procedure.

*All* data-management code must be run.

## 3. Descriptive statistics

Descriptive statistics data generated for the lying experiment. NB: descriptive figures and tables produced as part of the Appendix are included in sections 6 and 7 below.

## 4. Tables

Model generation and table formatting for the three tables that appear in the main text.

**NB:**
1. The bootstrapped ICC standard error estimates are reasonably computationally-intensive. Running the total set of eight function calls will take a substantial amount of time (~15 minutes on a 2017 MacBook Pro 2.3ghz i5).
2. Table 3 is generated in the separate indian_vignette_replication.R script

## 5. Figures

Model generation and production  of figures in the main text.

**NB:**
1. Figure code should be run after the table code has been executed.
2. Figure 1 is generated in the separate simulation_replication.R script
3. BART estimation is included in this section.

## 6. Appendix tables

Code for tables in the appendix.

## 7. Appendix figures

Code for figures in the appendix.
