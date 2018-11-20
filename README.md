# Multi-Modes for Detecting Experimental Measurement Error 

## Contents:
1. Code - Instructions and experimental code to replicate the experiments (both online and lab)
   * Lab code: includes z-Tree scripts and writen instructions that were read out load.
   * Online code: 
    ** Nodegame script 
    ** PDF of the instructions that were presented on screen
    ** Authentification script used to limite the re-entry of participants (each subject could particopante only once). 
2. Data - CSV and raw exports for each of the four separate experiments
3. R-scripts - All R code for generation of statistics, tables and figures. Key files:
   * replication.R - main replication file
   * dist_diff.R - function (sourced in replication.R) to calculate bootstrapped estimates of differences within CATE bands
4. Screenshots - of lab and online experiments

## Replication R code:

The structure of the replication.R file is as follows:
1. Code to format and standardise the raw data extracts from each experimental mode
2. Descriptive statistics code
3. All tables within the main body of text
4. All figures within the main body of text
5. All appendix tables
6. All appendix figures
