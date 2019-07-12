# README: Multi-Modes for Detecting Experimental Measurement Error

Raymond Duch, Denise Laroze, Thomas Robinson and Pablo Beramendi

Last edit: July 2019

## Contents:
1. **replication_scripts** - All R code for generation of statistics, tables and figures:
   * main_replication.R - main replication file
   * simulation_replication.R -  replication material for Figure 1
   * indian_vignette_replication.R - replication material for experimental results (Tables 3, B4, B5, and B6)

2. **data** - CSV exports for each separate experiment:
   * baseline_uk.csv - Lying behaviour lab experiment
   * CESS_Panel_DS_Feb2018.csv - Lying behaviour CESS Online UK experiment
   * lab_online_sync_edited.csv  - Lying behaviour lab online experiment
   * Mturk_DS_Sept2017.csv - Lying behaviour Mturk online experiment
   * mturk_exp.csv - India vignette experiment Mturk subjects
   * mturk_exp_incentivised.csv - India vignette experiment Mturk subjects with incentivisation
   * co_exp.csv - India vignette experiment CESS Online subjects

3. **replication_guide.md** - further guidance on reproducing the analysis in this article.

4. **screenshots** - of lab and online experiments

5. **tables** - .tex files of tables in main article and appendix

6. **figures** - image files of figures in main article and appendix

7. **experimental_code** - Instructions and experimental code to run the experiments (both online and lab):
* Lab code includes: z-Tree scripts and written instructions that were read out load.
* Online code includes:
a) Nodegame script
b) PDF of the instructions that were presented on screen
c) Authentication script used to limit the re-entry of participants (each subject could participate only once).

The following R packages and dependencies are required (including version numbers, as available on CRAN on 27/06/2019).

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

Code run using R version 3.6.0.

Approximate runtimes for R-scripts using a standard laptop (dual-core Intel Core i5-7360U @ 2.3GHz, 8.00GB RAM, MacOS 10.14.5):

* indian_vignette_replication.R -- < 1 minute
* simulation_replication.R --  < 1 minute
* main_replication.R -- ~23 minutes
