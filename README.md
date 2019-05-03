# Multi-Modes for Detecting Experimental Measurement Error 

Raymond Duch, Denise Laroze, Thomas Robinson and Pablo Beramendi (Nov 2018)


## Abstract:
Experiments should be designed to facilitate the detection of experimental measurement error. To this end, we advocate the implementation of identical experimental protocols employing diverse experimental modes.  We suggest iterative non-parametric estimation techniques for assessing the magnitude of heterogeneous treatment effects across these modes.  And we propose incorporating standard measurement strategies in the design that help assess whether any observed heterogeneity reflects experimental measurement error.  To illustrate our argument, we conduct, and analyze results from, four identical interactive experiments in the lab; online with subjects from the CESS lab subject pool; online with an online subject pool; and online with MTurk workers.


## Contents:
1. Code - Instructions and experimental code to replicate the experiments (both online and lab)
   * Lab code includes: z-Tree scripts and writen instructions that were read out load.
   * Online code includes:
   a) Nodegame script 
   b) PDF of the instructions that were presented on screen
   c) Authentification script used to limite the re-entry of participants (each subject could particopante only once). 
2. Data - CSV and raw exports for each of the four separate experiments
3. R-scripts - All R code for generation of statistics, tables and figures. Key files:
   * main_replication.R - main replication file
   * simulation_replication.R -  replication material for Figure 1
   * indian_vignette_replication.R - replication material for experimental results incl. Tables 6, B4, and B5
   * dist_diff.R - function (sourced into main_replication.R) to calculate bootstrapped estimates of differences within CATE bands
4. Screenshots - of lab and online experiments

## Replication R code:

The structure of the main_replication.R file is as follows:
0. Pre-requisites to run the file (packages, seed and graph colours)
1. Functions - function definitions used at various points in the code
2. Data Management - Code to format and standardise the raw data extracts from each experimental mode
3. Descriptive statistics code
4. All tables within the main body of text
5. All figures within the main body of text
6. All appendix tables
7. All appendix figures

N.B. Substantive analysis code is included by Table/Figure reference.
