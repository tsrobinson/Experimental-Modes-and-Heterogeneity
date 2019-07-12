######################################################################
##                                                                  ## 
##      PROJECT TITLE:    Multi-Modes for Detecting Experimental    ##
##                        Measurement Error                         ##
##                                                                  ## 
##      CODE AUTHORS:     THOMAS S. ROBINSON (JUNE 2019)            ##
##                        DENISE LAROZE (FEB 2017)                  ## 
##                                                                  ## 
##      EMAIL:            thomas.robinson@politics.ox.ac.uk         ##
##                        deniselaroze@gmail.com                    ##
##                                                                  ##
##      DESCRIPTION:      Main replication file                     ##
##                                                                  ##
######################################################################

#### 0. Pre-requisites ####
library(foreign)
library(broom)
library(ggpubr)
theme_set(theme_bw())
library(stargazer)
library(CBPS)
library(scales)
library(gridExtra)
library(effects)
library(plyr)
library(plm)
library(lmtest)
library(xtable)
library(FindIt)
library(BayesTree)
library(clusterSEs)
library(lme4)
library(sjstats)
library(dplyr)
library(ltm)
library(wesanderson)
library(stringr)

colours <- wes_palette("Royal1")
colours2 <- wes_palette(name = "Royal1", type = "continuous", n=21)

set.seed(89)

#setwd("your directory")


#### 1. Functions ####

## Risk consistency function
consist.risk<-function(df,name.risk){
  col_idx <- grep(name.risk, names(df))
  m1 <- as.matrix(df[,col_idx])
  m1<-as.data.frame(m1)
  m1$Consistent<-NA
  m1$risk.pref.consist<-NA
  for(i in 1:nrow(m1)){
    a<-m1[i,]
    vt<-rep(NA, ncol(a) )
    for(j in 2:ncol(a)){
      vt[j-1]<-ifelse(a[j-1]>a[j], "inconsistent", "OK")
      df$Consistent[i]<-ifelse("inconsistent" %in% vt, "No" ,  "Yes") 
      df$risk.pref.consist[i]<-ifelse("inconsistent" %in% vt,  NA, df$risk.pref[i])
    }
  }
  return(df)
}

## Normalise distribution - used in ability calculations
redist.fun <- function(x){(x-min(x, na.rm = T))/diff(range(x, na.rm = T))}

## Add brackets around rounded values, for producing Table 2
bracket <- function(vec) {
  for (i in 1:4){
    
    # Round standard error estimates
    rounded <- as.character(round(as.numeric(vec[i]), 2))
    
    # Add trailing zero where needed
    rounded <- ifelse(nchar(rounded) == 3, paste0(rounded,"0"), rounded)
    
    # Add in brackets
    vec[i] <- paste0("(",rounded,")")  
  }
  return(vec)
}

## Calculate normalized ability ranking
abilityCalc <- function(df) {
  
  # Recover mean RET performance per participant
  df$ability <- as.numeric(lapply(df$muID, function(x) mean(df[df$muID == x,]$ncorrectret, na.rm=T)))
  abilityDF <- group_by(df,muID)
  newDF <- summarise(abilityDF,
                     ability = mean(ability))
  
  # Rank ability scores
  newDF$rank <- rank(newDF$ability, na.last = "keep")
  df$rank <- as.numeric(lapply(df$muID, function(x) mean(newDF[newDF$muID == x,]$rank, na.rm=T)))
  
  # Normalize ranking
  df$rank <- redist.fun(df$rank)
  
  # Format gender
  df$Gender <- ifelse(df$Gender == "F",1,0)
  return(df)
}

## Calculate ICCs by mode, able to vary outcome of interest
icc_modes <- function(taxrate, auditrate, y) {
  
  # Select ICC model by DV
  model <- ifelse(y == "report.rate","report.rate ~ (1|muID)","ncorrectret ~ (1|muID)")
  
  # Run lmer models
  if (auditrate == 0) {
    fit_baseline_y_subset <- lmer(formula = model, data=baseline.uk[baseline.uk$taxrate == taxrate & baseline.uk$auditRate == auditrate,])
    fit_lab.online_y_subset <- lmer(formula = model, data=lab.online.sync[lab.online.sync$taxrate == taxrate & lab.online.sync$auditRate == auditrate,])
    fit_cess.online_y_subset <- lmer(formula = model, data=cess.online.panel[cess.online.panel$taxrate == taxrate & cess.online.panel$auditRate == auditrate,])
    fit_mturk_y_subset <- lmer(formula = model, data=mturk.ds[mturk.ds$taxrate == taxrate & mturk.ds$auditRate == auditrate,])
  } else {
    fit_baseline_y_subset <- lmer(formula = model, data=baseline.uk[baseline.uk$taxrate == taxrate & baseline.uk$auditRate != 0,])
    fit_lab.online_y_subset <- lmer(formula = model, data=lab.online.sync[lab.online.sync$taxrate == taxrate & lab.online.sync$auditRate != 0,])
    fit_cess.online_y_subset <- lmer(formula = model, data=cess.online.panel[cess.online.panel$taxrate == taxrate & cess.online.panel$auditRate != 0,])
    fit_mturk_y_subset <- lmer(formula = model, data=mturk.ds[mturk.ds$taxrate == taxrate & mturk.ds$auditRate != 0,])
  }
  
  # Recover ICC estimates from models, storing as a vector 
  return(c(icc(fit_baseline_y_subset)[[1]],
           icc(fit_lab.online_y_subset)[[1]],
           icc(fit_cess.online_y_subset)[[1]],
           icc(fit_mturk_y_subset)[[1]]))
}

## Bootstrap ICC estimate functions
# Credit: https://stats.stackexchange.com/questions/232252/intraclass-correlation-standard-error/232284
icc_se <- function(data, taxrate,auditrate,var) {
  
  # Subset data based on tax rate and auditrate passed to function
  if (auditrate == 0) {
    data <- data[data$taxrate == taxrate & data$auditRate == 0,]
  } else {
    data <- data[data$taxrate == taxrate & data$auditRate != 0,]
  }
  
  model <- ifelse(var == "report.rate","report.rate ~ (1 | muID)", "ncorrectret ~ (1 | muID)")
  
  # Bootstrapped lmer models for 1000 iterations
  dist <- c()
  for (i in 1:1000) {
    dummy <- data[sample(nrow(data), replace=TRUE),]
    dist[i] <- icc(lmer(model, data = dummy, REML = FALSE, 
                        control = lmerControl(optimizer ="Nelder_Mead")))[[1]]
  }
  
  # Recover s.e. estimate from vector of bootsrapped ICCs
  return(sd(dist))
}

## Calculate Cronbach alpha function
cronbach_fun <- function(data, integrity_measures) {
  df <- data[,c(integrity_measures,"muID")]
  df <- unique(df)
  return(cronbach.alpha(df[,integrity_measures], na.rm = T, CI = T))
}

#### 2. Data Management ####

## NB: This code block must be run prior to analysis.

## Load files

mturk.ds<-read.csv("data/Mturk_DS_Sept2017.csv") #
cess.online.panel <- read.csv("data/CESS_Panel_DS_Feb2018.csv")
cess.online.panel<-cess.online.panel[cess.online.panel$correct>0, ]
lab.online.sync <- read.csv("data/lab_online_sync_edited.csv")
baseline.uk<-read.csv("data/baseline_uk.csv")
baseline.uk<-baseline.uk[baseline.uk$auditrate<30, ] # Only sessions with 0 and 20% audit

## Data cleaning

#Cleaning up round number in lab version of the experiment
baseline.uk$round<-ifelse(baseline.uk$auditrate>0, baseline.uk$period+10, baseline.uk$period )

# Eliminating missing values
lab.online.sync$DictGive[lab.online.sync$DictGive==-1]<-NA

# Editing Age from logical to numeric
mturk.ds$age2<-mturk.ds$age
mturk.ds$age2[mturk.ds$age=="false"]<-NA
mturk.ds$Age<-as.numeric(levels(mturk.ds$age2))[mturk.ds$age2]


# Identifying and eliminating non-consistent risk preferences
# Eliminating observations null risk and integrity observations 
# caused by the way the online experiment is coded
cess.online.panel$risk.pref[cess.online.panel$risk.pref==0]<-NA
mturk.ds$risk.pref[mturk.ds$risk.pref==0]<-NA
lab.online.sync$risk.pref[lab.online.sync$risk.pref==0]<-NA

cess.online.panel$total.integrity[cess.online.panel$total.integrity==0]<-NA
mturk.ds$total.integrity[mturk.ds$total.integrity==0]<-NA
lab.online.sync$total.integrity[lab.online.sync$total.integrity==0]<-NA

#online.uk<-consist.risk(online.uk, "Risk_")
lab.online.sync<-consist.risk(lab.online.sync, "risk")
mturk.ds<-consist.risk(mturk.ds, "risk")
cess.online.panel<-consist.risk(cess.online.panel, "risk")

#rename variables
names(baseline.uk)[names(baseline.uk) == 'dec1'] <- 'Risk_1'
names(baseline.uk)[names(baseline.uk) == 'dec2'] <- 'Risk_2'
names(baseline.uk)[names(baseline.uk) == 'dec3'] <- 'Risk_3'
names(baseline.uk)[names(baseline.uk) == 'dec4'] <- 'Risk_4'
names(baseline.uk)[names(baseline.uk) == 'dec5'] <- 'Risk_5'
names(baseline.uk)[names(baseline.uk) == 'dec6'] <- 'Risk_6'
names(baseline.uk)[names(baseline.uk) == 'dec7'] <- 'Risk_7'
names(baseline.uk)[names(baseline.uk) == 'dec8'] <- 'Risk_8'
names(baseline.uk)[names(baseline.uk) == 'dec9'] <- 'Risk_9'
names(baseline.uk)[names(baseline.uk) == 'dec10'] <- 'Risk_10'

baseline.uk$risk.pref<-10-baseline.uk$safechoices
baseline.uk<-consist.risk(baseline.uk, "Risk_")  

cess.online.panel$risk.pref[cess.online.panel$risk.pref==0]<-NA
mturk.ds$risk.pref[mturk.ds$risk.pref==0]<-NA
lab.online.sync$risk.pref[lab.online.sync$risk.pref==0]<-NA

cess.online.panel$total.integrity[cess.online.panel$total.integrity==0]<-NA
mturk.ds$total.integrity[mturk.ds$total.integrity==0]<-NA
lab.online.sync$total.integrity[lab.online.sync$total.integrity==0]<-NA

## change the unit of variables
mturk.ds$DictGive.normal <- mturk.ds$DictGive/1000
mturk.ds$total.integrity.normal <- (mturk.ds$total.integrity)/40
mturk.ds$risk.pref.normal <- (mturk.ds$risk.pref.consist)/10

lab.online.sync$DictGive.normal <- lab.online.sync$DictGive/1000
lab.online.sync$total.integrity.normal <- (lab.online.sync$total.integrity)/40
lab.online.sync$risk.pref.normal <- (lab.online.sync$risk.pref.consist)/10

cess.online.panel$DictGive.normal <- cess.online.panel$DictGive/1000
cess.online.panel$total.integrity.normal <- (cess.online.panel$total.integrity)/40
cess.online.panel$risk.pref.normal <- (cess.online.panel$risk.pref.consist)/10 


### adapting Baseline uk data
m1 <- as.matrix(baseline.uk[, c("publictransport","taxes", "drivingfast", "moneyfound",                 
                                "lying", "accidentaldamage", "litter",                     
                                "drivingalcohol", "jobapplication", "buyingstolen")])
class(m1)<-"numeric"

baseline.uk$total.integrity<-rowSums(m1, na.rm = T)

baseline.uk$DictGive.normal <- baseline.uk$offerdg/1000   
baseline.uk$risk.pref.normal <- baseline.uk$risk.pref.consist/10
baseline.uk$total.integrity.normal <- (baseline.uk$total.integrity)/40
baseline.uk$report.rate <- baseline.uk$declared/baseline.uk$profitret

baseline.uk$treat<-NA
baseline.uk$treat[baseline.uk$auditrate==0]<-2
baseline.uk$treat[baseline.uk$auditrate==20]<-1

baseline.uk$Gender_lab[baseline.uk$gender==0]<-"F"
baseline.uk$Gender_lab[baseline.uk$gender==1]<-"M"

### Number of correct responses
lab.online.sync$ncorrectret <- lab.online.sync$correct
mturk.ds$ncorrectret<-mturk.ds$correct
cess.online.panel$ncorrectret<-cess.online.panel$correct

## Rename variables
baseline.uk$muID<-paste0("baseline", baseline.uk$subj_id)
names(baseline.uk)[names(baseline.uk)=="age_subject"] <- "Age"
names(baseline.uk)[names(baseline.uk)=="safechoices"] <- "risk.pref"
names(baseline.uk)[names(baseline.uk)=="profitret"] <- "prelimGain"
names(baseline.uk)[names(baseline.uk)=="offerdg"] <- "DictGive"
names(baseline.uk)[names(baseline.uk)=="Gender_lab"] <- "Gender"
names(baseline.uk)[names(baseline.uk)=="auditrate"] <- "auditRate"

names(lab.online.sync)[names(lab.online.sync)=="age"] <- "Age"
names(lab.online.sync)[names(lab.online.sync)=="gender"] <- "Gender"
names(lab.online.sync)[names(lab.online.sync)=="taxRate"] <- "taxrate"

names(mturk.ds)[names(mturk.ds)=="gender"] <- "Gender"
names(mturk.ds)[names(mturk.ds)=="taxRate"] <- "taxrate"

names(cess.online.panel)[names(cess.online.panel)=="age"] <- "Age"
names(cess.online.panel)[names(cess.online.panel)=="gender"] <- "Gender"
names(cess.online.panel)[names(cess.online.panel)=="taxRate"] <- "taxrate"

# Vector of variables used:
vars<-c( "muID", "ncorrectret" ,"Gender", "Age", "DictGive" ,"DictGive.normal", "total.integrity", "total.integrity.normal", 
         "risk.pref.normal", "risk.pref", "prelimGain", "report.rate", "treat", "taxrate", "round","auditRate"
)

# Gen. consistent set of dataframes
o.sync<-lab.online.sync[, vars]
o.sync$sample<-"Online Lab"
b.s<-baseline.uk[, vars]
b.s$sample<-"Lab"
mt.s<-mturk.ds[, vars]
mt.s$sample<-"Mturk"

cp.s<-cess.online.panel[, vars]
cp.s$sample<-"CESS Online UK"

p.data<-rbind(o.sync, b.s, mt.s, cp.s)

# Assign unique subject IDs
p.data$muID<-paste0(p.data$sample, p.data$muID)

# Remove invalid audit rates
p.data<-p.data[!is.na(p.data$auditRate),]

# Remove temporary dataframes
rm(o.sync, b.s, mt.s, cp.s)


#### 3. Descriptive Statistics (reported in the appendix) #### 

## DS: Consistency in risk preferences 
prop.table(table(baseline.uk$Consistent ))
prop.table(table(lab.online.sync$Consistent))
prop.table(table(mturk.ds$Consistent ))
prop.table(table(cess.online.panel$Consistent ))


## DS: Mean Give in dictator game
ddply(p.data, ~ sample, summarize, 
      m.report = mean(DictGive, na.rm=T)
)

# Comparisons across modes/groups
lab<-c( baseline.uk$DictGive, lab.online.sync$DictGive)
others<-c(mturk.ds$DictGive, cess.online.panel$DictGive)

wilcox.test(lab, others,
            alternative = "two.sided",conf.level = 0.95)

t.test(lab, others, alternative ="two.sided", conf.level = 0.95)


# General comparisons
pairwise.t.test(p.data$DictGive, p.data$sample, # p.adjust.method = p.adjust.methods,
                pool.sd = FALSE, paired = F,
                alternative = c("two.sided"))

pairwise.wilcox.test(p.data$DictGive, p.data$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)


## Preliminary Gains

ddply(p.data, ~ sample, summarize, 
      m.prelimgain = mean(prelimGain, na.rm=T),
      m.correct.ret= mean(ncorrectret, na.rm=T)
)


#### 4. Tables ####

## Table 1 - GLM model with clustered s.e's

# Run ability ranking function for each mode separately
lab.online.sync.ab <- abilityCalc(lab.online.sync[,c("muID","report.rate","ncorrectret","taxrate","treat","Age","Gender")])
baseline.uk.ab <- abilityCalc(baseline.uk[,c("muID","report.rate","ncorrectret","taxrate","treat","Age","Gender")])
mturk.ds.ab <- abilityCalc(mturk.ds[,c("muID","report.rate","ncorrectret","taxrate","treat","Age","Gender")])
cess.online.panel.ab <- abilityCalc(cess.online.panel[,c("muID","report.rate","ncorrectret","taxrate","treat","Age","Gender")])

# Run GLM models on above data frames
model.1.lab.online.sync <- glm(report.rate ~ rank + factor(taxrate) +factor(treat) + Age + factor(Gender), data=lab.online.sync.ab)
model.1.baseline <- glm(report.rate ~ rank + factor(taxrate) +factor(treat) + Age + factor(Gender), data=baseline.uk.ab)
model.1.mt.ds <- glm(report.rate ~ rank + factor(taxrate) +factor(treat) + Age + factor(Gender), data=mturk.ds.ab)
model.1.cp.ds <- glm(report.rate ~ rank + factor(taxrate) +factor(treat) + Age + factor(Gender), data=cess.online.panel.ab)

# Basic standard error clustering procedure
model.1.lab.online.sync.cluster <- coeftest(model.1.lab.online.sync, vcov=vcovHC(model.1.lab.online.sync, cluster = "muID", type = "HC0"))
model.1.baseline.cluster <- coeftest(model.1.baseline, vcov=vcovHC(model.1.baseline, cluster = "muID", type = "HC0"))
model.1.mt.ds.cluster <- coeftest(model.1.mt.ds, vcov=vcovHC(model.1.mt.ds, cluster = "muID", type = "HC0"))
model.1.cp.ds.cluster <- coeftest(model.1.cp.ds, vcov=vcovHC(model.1.cp.ds, cluster = "muID", type = "HC0"))

# Table output

models <- c( "Lab", "Online Lab", "Online UK",  "Mturk")
covariates <- c( "Ability Rank" , "20\\% Deduction","30\\% Deduction","No Audit","Age","Gender (1 $=$ Female)","Constant")

stargazer(model.1.baseline, model.1.lab.online.sync, model.1.cp.ds, model.1.mt.ds,
          column.labels = models,
          dep.var.caption = "Mode",
          dep.var.labels = "",
          model.numbers = F,
          covariate.labels = covariates,
          title = "GLM estimation on percent declared",
          label = "table:glm",
          digits = 2,
          no.space=TRUE,
          # NB: standard errors from clustering procedure used:
          se = list(model.1.baseline.cluster[,2],
                    model.1.lab.online.sync.cluster[,2],
                    model.1.cp.ds.cluster[,2],
                    model.1.mt.ds.cluster[,2]),
          keep.stat = c("n"),
          star.cutoffs  =  c(NA,NA,NA),
          omit.table.layout = "n",
          out="tables/table_1.tex")

## Table 2 - ICCs by mode, for outcome and RET variables

# Rename column so it is consistent across mode dataframes
colnames(baseline.uk)[colnames(baseline.uk) == "auditrate"] <- "auditRate"

# Run icc calculations:
# Tax rate = 10, audit rate = 0
reportrate_subset_10_0_icc <- icc_modes(taxrate = 10, auditrate = 0, y = "report.rate")
ret_subset_10_0_icc <- icc_modes(taxrate = 10, auditrate = 0, y = "ncorrectret")

# Tax rate = 30, audit rate = 0
reportrate_subset_30_0_icc <- icc_modes(taxrate = 30, auditrate = 0, y = "report.rate")
ret_subset_30_0_icc <- icc_modes(taxrate = 30, auditrate = 0, y = "ncorrectret")

# Audit rate != 0, tax rate = 10 
reportrate_subset_10_a_icc <- icc_modes(taxrate = 10, auditrate = ">0", y = "report.rate")
ret_subset_10_a_icc <- icc_modes(taxrate = 10, auditrate = ">0", y = "ncorrectret")

# Audit rate != 0, tax rate = 30
reportrate_subset_30_a_icc <- icc_modes(taxrate = 30, auditrate = ">0", y= "report.rate")
ret_subset_30_a_icc <- icc_modes(taxrate = 30, auditrate = ">0", y= "report.rate")


# Get bootstrapped standard errors for ICC estimates for report rate, by mode:
baseline_y <- c(icc_se(baseline.uk,10,0,"report.rate"),
                icc_se(baseline.uk,30,0,"report.rate"),
                icc_se(baseline.uk,10,1,"report.rate"),
                icc_se(baseline.uk,30,1,"report.rate"))

lab.online_y <- c(icc_se(lab.online.sync,10,0,"report.rate"),
                  icc_se(lab.online.sync,30,0,"report.rate"),
                  icc_se(lab.online.sync,10,1,"report.rate"),
                  icc_se(lab.online.sync,30,1,"report.rate"))

cess.online_y <- c(icc_se(cess.online.panel,10,0,"report.rate"),
                   icc_se(cess.online.panel,30,0,"report.rate"),
                   icc_se(cess.online.panel,10,1,"report.rate"),
                   icc_se(cess.online.panel,30,1,"report.rate"))

mturk_y <- c(icc_se(mturk.ds,10,0,"report.rate"),
             icc_se(mturk.ds,30,0,"report.rate"),
             icc_se(mturk.ds,10,1,"report.rate"),
             icc_se(mturk.ds,30,1,"report.rate"))

# Get bootstrapped standard errors for ICC estimates for RET, by mode:
baseline_ret <- c(icc_se(baseline.uk,10,0,"ncorrectret"),
                  icc_se(baseline.uk,30,0,"ncorrectret"),
                  icc_se(baseline.uk,10,1,"ncorrectret"),
                  icc_se(baseline.uk,30,1,"ncorrectret"))

lab.online_ret <- c(icc_se(lab.online.sync,10,0,"ncorrectret"),
                    icc_se(lab.online.sync,30,0,"ncorrectret"),
                    icc_se(lab.online.sync,10,1,"ncorrectret"),
                    icc_se(lab.online.sync,30,1,"ncorrectret"))

cess.online_ret <- c(icc_se(cess.online.panel,10,0,"ncorrectret"),
                     icc_se(cess.online.panel,30,0,"ncorrectret"),
                     icc_se(cess.online.panel,10,1,"ncorrectret"),
                     icc_se(cess.online.panel,30,1,"ncorrectret"))

mturk_ret <- c(icc_se(mturk.ds,10,0,"ncorrectret"),
               icc_se(mturk.ds,30,0,"ncorrectret"),
               icc_se(mturk.ds,10,1,"ncorrectret"),
               icc_se(mturk.ds,30,1,"ncorrectret"))

# Build output tables:

# Add brackets around standard errors
baseline_y_b <- bracket(baseline_y)
lab.online_y_b <- bracket(lab.online_y)
cess.online_y_b <- bracket(cess.online_y)
mturk_y_b <- bracket(mturk_y)
baseline_ret_b <- bracket(baseline_ret)
lab.online_ret_b <- bracket(lab.online_ret)
cess.online_ret_b <- bracket(cess.online_ret)
mturk_ret_b <- bracket(mturk_ret)

modes <- c("Lab","Lab Online","CESS Onine","MTurk")

# Combine and round ICC estimates for report rate into dataframe
y_table <- round(cbind(reportrate_subset_10_0_icc,
                       reportrate_subset_30_0_icc,
                       reportrate_subset_10_a_icc,
                       reportrate_subset_30_a_icc),2)

# Add in trailing zeros for consistency when printing table to latex
y_table <- ifelse(nchar(y_table[,1:4]) == 3, paste0(y_table[,1:4],"0"),y_table[,1:4])


tax <- c("10%","30%","10%","30%")
audit <- c("No","No","Yes","Yes")

# Add standard errors underneath corresponding ICC estimates
y_table <- rbind(y_table[1,],
                 baseline_y_b,
                 y_table[2,],
                 lab.online_y_b,
                 y_table[3,],
                 cess.online_y_b,
                 y_table[4,],
                 mturk_y_b)

y_table <- rbind(y_table, tax, audit)

# Add in additional model information:
modes <- c("Lab","","Lab Online","","CESS Onine","","MTurk","","Tax Rate","Audited?")
y_table <- cbind(modes,y_table)
colnames(y_table) <- c("Mode","(1)","(2)","(3)","(4)")


# Formatting same as above, but for RET ICCs
ret_table <- round(cbind(ret_subset_10_0_icc,
                         ret_subset_30_0_icc,
                         ret_subset_10_a_icc,
                         ret_subset_30_a_icc),2)

ret_table <- rbind(ret_table[1,],
                   baseline_ret_b,
                   ret_table[2,],
                   lab.online_ret_b,
                   ret_table[3,],
                   cess.online_ret_b,
                   ret_table[4,],
                   mturk_ret_b)

ret_table <- ifelse(nchar(ret_table[,1:4]) == 3, paste0(ret_table[,1:4],"0"),ret_table[,1:4])

ret_table <- rbind(ret_table, tax, audit)

colnames(ret_table) <- c("(1)","(2)","(3)","(4)")

# Combine report rate and RET ICC dataframes into one table
icc_table <- cbind(y_table, ret_table)

# Print table to .tex file
addtorow_icc <- list()
addtorow_icc$pos <- list(-1)
addtorow_icc$command <- c("\\hline & \\multicolumn{4}{c}{\\textbf{Report Rate (Outcome)}} & \\multicolumn{4}{c}{\\textbf{RET}} \\\\ \\cline{2-9}")

print(xtable(icc_table, row.names = FALSE, 
             caption = "Comparison of ICCs across modes for both outcome and RET", 
             label = "tab:icc",
             align = "llllll|llll"),
      include.rownames = FALSE, hline.after = c(0,8,10),
      add.to.row = addtorow_icc,
      file = "tables/table_2.tex")

## Table 3

# See separate R script "indian_vignette_replication.R"

#### 5. Figures ####

## Figure 1 - simulation of measurement error

# See separate R script "simulation_replication.R"

## Figure 2 - BART plot

set.seed(89)

# Data set up including calculating ability rank
df <- p.data
df$treat.het <- ifelse(is.na(df$taxrate),NA,ifelse(df$taxrate > 10,1,0))
df$Gender <- ifelse(df$Gender == "F",1,0)
df$sample <- as.factor(df$sample)

ability <- df %>% dplyr::group_by(muID) %>%
  summarise(ind_ability = mean(ncorrectret, na.rm=T)) %>%
  mutate(ability = rank(ind_ability, na.last = "keep")) %>%
  mutate(ability = redist.fun(ability)) %>%
  select(-ind_ability)

df <- left_join(df, ability, by = "muID")

# Define model variables incl. outcome as column 1
vars <- c("report.rate","treat.het","ability","sample","Age","Gender")

df <- df[,vars]
df <- df[complete.cases(df),]

# Separate outcome and training data
y <- df$report.rate
train <- df[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates and format into dataframe
# Logic: Take predictions for those actually treated and minus counterfactual
#        Then take counterfactually treated and deduct prediction for those actually in control
CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
          bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)
covars <- rbind(train[train$treat.het == 1,c(2:5)], test[test$treat.het==1,c(2:5)])

CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))

# Descriptive results reported in main text:
mean(CATE_df$CATE)
summary(CATE_df$CATE)

# Proportion of CATEs that are negative:
sum(CATE_df$CATE < 0)/nrow(CATE_df)
sum(CATE_df$CATE < -0.07)/nrow(CATE_df)

# Lab participant: prop. below -0.07
sum(CATE_df$CATE < -0.07 & CATE_df$sample %in% c("Lab", "Online Lab"))/sum(CATE_df$sample %in% c("Lab", "Online Lab"))

# Online participants: prop. above -0.07 and prop. above 0
sum(CATE_df$CATE > -0.07 & CATE_df$sample %in% c("CESS Online UK", "Mturk"))/sum(CATE_df$sample %in% c("CESS Online UK", "Mturk"))
sum(CATE_df$CATE > 0 & CATE_df$sample %in% c("CESS Online UK", "Mturk"))/sum(CATE_df$sample %in% c("CESS Online UK", "Mturk"))

# CATE Heterogeneity plot
hist <- CATE_df

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,5220))

# Mode histogram 

modePlot <- ggplot(hist, aes(x=id, fill=sample)) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  scale_fill_manual(name="Mode", values=colours) +
  labs(y = "Count", x = "Individual") +
  scale_x_continuous(limits = c(0,5220))

# Combine all plots into one chart
figure_2 <- ggarrange(effectsPlot, modePlot,
                    ncol = 1, nrow = 2, heights = c(2,2))

ggsave(figure_2, filename = "figures/figure_2.pdf", device = "pdf", height = 8, width = 6, dpi = 300)

## Figure 3 - comparison of report rates

df <- p.data[p.data$treat == 2,]

cdata <- ddply(df, c("sample"), summarise,
               mean.report.rate = mean(report.rate, na.rm=T))

plot_df <- cdata
plot_df$facet <- "All Modes"
plot_df$age <- "All"

# Generate binary age category
df$age <- ifelse(is.na(df$Age),NA,ifelse(df$Age < 30, "Age < 30", "Age > 30"))

# Summarise report rates by mode and age category
cdata <- ddply(df[df$sample %in% c("CESS Online UK","Mturk"),], 
               c("sample","age"), summarise,
               mean.report.rate = mean(report.rate, na.rm=T))

cdata$facet <- "Online Modes Age Breakdown"

plot_df <- rbind(plot_df,cdata)
plot_df <- plot_df[complete.cases(plot_df),]
plot_df$facet <- factor(plot_df$facet, levels=c("All Modes", "Online Modes Age Breakdown"))
plot_df$sample <- factor(plot_df$sample, levels=c("Lab","Online Lab", "CESS Online UK", "Mturk"))

# Get mode-average CATEs, and split by age for online samples
mode_cate_all<- CATE_df %>% 
  group_by(sample) %>%
  summarise(cate_mode = round(mean(CATE),3)) %>%
  mutate(age_bin = "All")

mode_cate <- CATE_df %>% 
  # Get Age
  mutate(age_bin = ifelse(Age < 30, "Age < 30","Age > 30")) %>%
  group_by(sample, age_bin) %>%
  summarise(cate_mode = round(mean(CATE),3)) %>%
  filter(sample %in% c("CESS Online UK","Mturk")) %>%
  bind_rows(mode_cate_all)

# Combine mean report rate and mode-average CATE information
plot_df <- plot_df %>%
  left_join(mode_cate, by = c("sample", "age" = "age_bin"))

# Plot:
figure_3 <- ggplot(plot_df, aes(x = sample, y=mean.report.rate, fill=age)) +
  facet_grid( ~ facet, space = "free", scales = "free") +
  geom_col(position = "dodge") +
  geom_text(aes(label = cate_mode), vjust = -0.5, position = position_dodge(0.9),
            size = 2.9) +
  labs(x = "Age", y = "Mean Report Rate", caption = "Corresponding group average CATE reported above each column") +
  scale_fill_manual("", values = colours[c(4,2,1)]) +
  # scale_fill_brewer(palette = "Set1") +
  theme(legend.position="bottom",
        plot.caption = element_text(hjust = 0.5))

ggsave(plot = figure_3, "figures/figure_3.pdf", device = "pdf", height = 5, width = 8, dpi = 300)


#### 6. Appendix Tables ####

## Table A1 and tax experiment numbers page 3-4

tab_a1 <- p.data[!is.na(p.data$auditRate),]
tab_a1$auditRate[tab_a1$auditRate==20]<- 0.2                               
sum.table<-ddply(tab_a1, c("sample"), summarize,
                 DG="Yes",
                 Risk="Yes",
                 auditrate=as.character(list(sort(na.omit(unique(auditRate))))),
                 taxrate=as.character(list(sort(na.omit(unique(taxrate))))),
                 m.report = mean(report.rate, na.rm=T),
                 n = length(unique(muID)),
                 n_response = length(report.rate)
)

gender.t<-prop.table(table(tab_a1$sample, tab_a1$Gender),1)
gender.t<-as.data.frame(gender.t)

sum.table$pc.female<-gender.t[1:4,3]
sum.table$pc.male<-gender.t[5:8,3]

place<-nrow(sum.table)+1
sum.table[place, "sample"]<-"All"
sum.table[place, "DG"]<-"Yes"
sum.table[place, "Risk"]<-"Yes"
sum.table[place, "auditrate"]<-as.character(list(unique(tab_a1$auditRate)))
sum.table[place, "taxrate"]<-as.character(list(sort(na.omit(unique(tab_a1$taxrate)))))
sum.table[place, "m.report"]<-mean(tab_a1$report.rate, na.rm=T)
sum.table[place, "n_response"]<-length(tab_a1$report.rate)


gender<-prop.table(table (tab_a1$Gender))
sum.table[place, "pc.female"]<-gender[1]
sum.table[place, "pc.male"]<-gender[2]

n<-length(unique(tab_a1$muID))
sum.table[place, "n"]<-n


names(sum.table) <- c('Mode','DG', 'Risk' ,'Audit Rate' ,"Tax Rate", "Report Rate" ,"# Subjects", "# Obs", "% Female", "% Male" )

addtorow_sum <- list()
addtorow_sum$pos <- list(0, 0)
addtorow_sum$command <- c("Mode & DG & Risk & Audit  & Tax  & Report  & \\#  & \\#  & \\%  & \\%  \\\\\n", 
                          "&  &  &  Rate &  Rate &  Rate &  Subjects & Obs & Female &  Male \\\\\n")

print(xtable(sum.table,
             caption = "Summary of experimental treatments",
             label = "tab:sum"),
      size = "\\scriptsize",
      type="latex", 
      file=("tables/table_a1.tex"),
      add.to.row = addtorow_sum,
      table.placement = "H",
      include.rownames=FALSE,
      include.colnames=FALSE)


## Table A2

# Wild cluster bootstrapped procedure
set.seed(89)

model.1.lab.online.sync.wildse <- cluster.wild.glm(model.1.lab.online.sync, lab.online.sync.ab, cluster = ~ muID, ci.level = 0.95,
                                                   boot.reps = 1000, report = TRUE, prog.bar = TRUE)

model.1.baseline.wildse <- cluster.wild.glm(model.1.baseline, dat=baseline.uk.ab, cluster= ~ muID, ci.level = 0.95,
                                            boot.reps = 1000, report = TRUE, prog.bar = TRUE)

model.1.mt.ds.wildse <- cluster.wild.glm(model.1.mt.ds, dat=mturk.ds.ab, cluster=  ~ muID, ci.level = 0.95,
                                         boot.reps = 1000, report = TRUE, prog.bar = TRUE)

model.1.cp.ds.wildse <- cluster.wild.glm(model.1.cp.ds, dat=cess.online.panel.ab, cluster=  ~ muID, ci.level = 0.95,
                                         boot.reps = 1000, report = TRUE, prog.bar = TRUE)


## Pairs cluster bootstraped procedure
set.seed(89)

model.1.lab.online.sync.pcbse <- cluster.bs.glm(mod=model.1.lab.online.sync, dat=lab.online.sync.ab, cluster= ~ muID, ci.level = 0.95,
                                                boot.reps = 1000, report = TRUE, prog.bar = TRUE)

model.1.baseline.pcbse <- cluster.bs.glm(mod=model.1.baseline, dat=baseline.uk.ab, cluster= ~ muID, ci.level = 0.95,
                                         boot.reps = 1000, report = TRUE, prog.bar = TRUE)

model.1.mt.ds.pcbse <- cluster.bs.glm(mod=model.1.mt.ds, dat=mturk.ds.ab, cluster= ~ muID, ci.level = 0.95,
                                      boot.reps = 1000, report = TRUE, prog.bar = TRUE)

model.1.cp.ds.pcbse <- cluster.bs.glm(mod=model.1.cp.ds, dat=cess.online.panel.ab, cluster= ~ muID, ci.level = 0.95,
                                      boot.reps = 1000, report = TRUE, prog.bar = TRUE)

# Wild cluster 
bs <- data.frame(coefficient = dimnames(model.1.baseline.wildse$p.values)[[1]],
                 p = model.1.baseline.wildse$p.values,
                 mode = "Lab")
lab.online <- data.frame(coefficient = dimnames(model.1.lab.online.sync.wildse$p.values)[[1]],
                         p = model.1.lab.online.sync.wildse$p.values,
                         mode = "Online Lab")
cp.ds <- data.frame(coefficient = dimnames(model.1.cp.ds.wildse$p.values)[[1]],
                    p = model.1.cp.ds.wildse$p.values,
                    mode = "Online UK")
mt.ds <- data.frame(coefficient = dimnames(model.1.mt.ds.wildse$p.values)[[1]],
                    p = model.1.mt.ds.wildse$p.values,
                    mode = "MTurk")

clusterTable.wild <- rbind(bs,lab.online,cp.ds,mt.ds)
rm(bs,lab.online.cp.ds,mt.ds)


# PCBSE cluster 
bs <- data.frame(coefficient = dimnames(model.1.baseline.pcbse$p.values)[[1]],
                 p = model.1.baseline.pcbse$p.values,
                 mode = "Lab")
lab.online <- data.frame(coefficient = dimnames(model.1.lab.online.sync.pcbse$p.values)[[1]],
                         p = model.1.lab.online.sync.pcbse$p.values,
                         mode = "Online Lab")
cp.ds <- data.frame(coefficient = dimnames(model.1.cp.ds.pcbse$p.values)[[1]],
                    p = model.1.cp.ds.pcbse$p.values,
                    mode = "Online UK")
mt.ds <- data.frame(coefficient = dimnames(model.1.mt.ds.pcbse$p.values)[[1]],
                    p = model.1.mt.ds.pcbse$p.values,
                    mode = "MTurk")

clusterTable.pcbse <- rbind(bs,lab.online,cp.ds,mt.ds)
rm(bs,lab.online.cp.ds,mt.ds)

clusterTable.wild <- reshape(clusterTable.wild, direction = "wide", timevar = "mode", idvar = "coefficient")
clusterTable.pcbse <- reshape(clusterTable.pcbse, direction = "wide", timevar = "mode", idvar = "coefficient")

clusterTable <- cbind(clusterTable.wild,clusterTable.pcbse[,c(2:5)])
clusterTable$coefficient <- c("Constant","Ability Rank","20% Deduction","30% Deduction","No Audit","Age","Gender (1 = Female)")

addtorow <- list()
addtorow$pos <- list(0, 0, 0, 0,0)
addtorow$command <- c("\\hline \n",
                      "& \\multicolumn{4}{c}{\\textbf{Wild}} & \\multicolumn{4}{c}{\\textbf{PCB}}\\\\\n",
                      "\\hline \n",
                      "& & Online & Online & & & Online & Online & \\\\\n",
                      "& Lab & Lab & UK & MTurk & Lab & Lab & UK & MTurk\\\\\n")

tab4 <- xtable(clusterTable, caption = "Wild and PCB clustered p-values", label = "table:cluster")
align(tab4) <- "llcccc|cccc"
print(tab4,include.rownames = FALSE, include.colnames = FALSE, 
      add.to.row = addtorow,
      file = "tables/table_a2.tex", 
      floating.environment = "table",
      table.placement =  "H")

## Table A3

set.seed(89)
df <- p.data

# Assign treatment value to subjects based on RET performance
df$treat.het <- df$treat.het <- ifelse(is.na(df$taxrate),NA,ifelse(df$taxrate > 10,1,0))
df$Gender <- ifelse(df$Gender == "F",1,0)
df$onlinelab <- ifelse(is.na(df$sample),NA,ifelse(df$sample == "Online Lab",1,0))
df$mturk <- ifelse(is.na(df$sample),NA,ifelse(df$sample == "Mturk",1,0))
df$cess <- ifelse(is.na(df$sample),NA,ifelse(df$sample == "CESS Online UK",1,0))

ability <- df %>% dplyr::group_by(muID) %>%
  summarise(ind_ability = mean(ncorrectret, na.rm=T)) %>%
  mutate(ability = rank(ind_ability, na.last = "keep")) %>%
  mutate(ability = redist.fun(ability)) %>%
  select(-ind_ability)


df <- left_join(df, ability, by = "muID")

## Not run:
# First, find tuning parameters
# F_search <- FindIt(model.treat = report.rate ~ treat.het,
#                model.main = ~ onlinelab + mturk + cess + ability + Age + Gender,
#                model.int = ~ onlinelab + mturk + cess + ability + Age + Gender,
#                data = df,
#                type = "continuous",
#                treat.type = "single",
#                search.lambdas = TRUE)
##

# Use tuning parameters for final model estimate
F1 <- FindIt(model.treat = report.rate ~ treat.het,
             model.main = ~ onlinelab + mturk + cess + ability + Age + Gender,
             model.int = ~ onlinelab + mturk + cess + ability + Age + Gender,
             data = df,
             type = "continuous",
             treat.type = "single",
             search.lambdas = FALSE,
             lambdas = c(-8.0125,-8.0075))
summary(F1)

pred  <-  predict(F1)
pred_findit <- pred$data

F1_summary <- summary(F1)

var_order <- c("Treatment",
               "MTurk",
               "Age",
               "$\\text{Age}^2$",
               "Gender",
               "Ability",
               "$\\text{Ability}^2$",
               "Treatment $\\times$ MTurk",
               "Treatment $\\times$ CESS Online UK",
               "Treatment $\\times$ Online Lab",
               "Treatment $\\times$ Ability",
               "Treatment $\\times$ Age",
               "Treatment $\\times$ Gender",
               "Online Lab $\\times$ Ability",
               "Online Lab $\\times$ Gender",
               "MTurk $\\times$ Ability",
               "MTurk $\\times$ Age",
               "MTurk $\\times$ Gender",
               "CESS Online UK $\\times$ Ability",
               "CESS Online UK $\\times$ Age",
               "Ability $\\times$ Age",
               "Ability $\\times$ Gender",
               "Age $\\times$ Gender",
               "Treatment $\\times$ Online Lab $\\times$ Ability",
               "Treatment $\\times$ Online Lab $\\times$ Age",
               "Treatment $\\times$ Online Lab $\\times$ Gender",
               "Treatment $\\times$ MTurk $\\times$ Ability",
               "Treatment $\\times$ MTurk $\\times$ Gender",
               "Treatment $\\times$ CESS Online UK $\\times$ Ability",
               "Treatment $\\times$ CESS Online UK $\\times$ Age",
               "Treatment $\\times$ CESS Online UK $\\times$ Gender",
               "Treatment $\\times$ Ability $\\times$ Age",
               "Treatment $\\times$ Ability $\\times$ Gender",
               "Treatment $\\times$ Age $\\times$ Gender",
               "Treatment $\\times$ $\\text{Ability}^2$",
               "Treatment $\\times$ $\\text{Age}^2$",
               "Intercept")

table_a3 <- as.data.frame(F1_summary$coefficients) %>%
  mutate(Variable = row.names(.))  %>%
  rename(Coefficient = "F1_summary$coefficients") %>%
  mutate(Coefficient = as.numeric(Coefficient),
         
         Variable = stringr::str_replace(Variable,":"," "),
         Variable = stringr::str_replace(Variable,":"," "),
         Variable = stringr::str_to_title(Variable),
         Variable = stringr::str_replace(Variable," ",":"),
         Variable = stringr::str_replace(Variable," ",":"),
         
         Variable = stringr::str_replace(Variable, ":"," $\\\\times$ "),
         Variable = stringr::str_replace(Variable, ":"," $\\\\times$ "),
         
         Variable = stringr::str_replace(Variable, "Mturk","MTurk"),
         Variable = stringr::str_replace(Variable, "Onlinelab","Online Lab"),
         Variable = stringr::str_replace(Variable, "Cess","CESS Online UK"),
         Variable = stringr::str_replace(Variable, "Treat","Treatment"),
         
         Variable = stringr::str_replace(Variable,"Ability.2","$\\\\text{Ability}^2$"),
         Variable = stringr::str_replace(Variable,"Age.2","$\\\\text{Age}^2$")
  ) %>%
  select(Variable, Coefficient) %>%
  slice(match(var_order, Variable))

table_a3[nrow(table_a3)+1,] <- c("\\textit{ATE}",F1$ATE)

table_a3$Coefficient <- as.numeric(table_a3$Coefficient)

print(xtable(table_a3,
             digits = 3,
             caption = "Heterogeneous treatment coefficients and interactions using iterated LASSO model", 
             label = "tab:hetero",
             align = "llc",
             display = c("f","s","f")),
      sanitize.text.function = function(x) {x},
      table.placement = "",
      include.rownames = FALSE,
      hline.after=c(-1,0,
                    nrow(table_a3)-1, 
                    nrow(table_a3),nrow(table_a3)),
      file = "tables/table_a3.tex")

## Table B4

# See separate R script "indian_vignette_replication.R"


## Table B5

# See separate R script "indian_vignette_replication.R"


#### 7. Appendix Figures ####

## Figure A1
pairwise.t.test(p.data$Age, p.data$sample, p.adjust.method = p.adjust.methods,
                pool.sd = FALSE,
                alternative = c("two.sided"))

pairwise.wilcox.test(p.data$Age, p.data$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)



lab<-p.data$Age[p.data$sample=="Lab"]
online.lab<-p.data$Age[p.data$sample=="Online Lab"]
online.panel<-p.data$Age[p.data$sample=="CESS Online UK"]
Mturk<-p.data$Age[p.data$sample=="Mturk"]


ks.test(lab, online.lab ,
        alternative = c("two.sided"),
        exact = NULL)

ks.test(online.panel, Mturk,
        alternative = c("two.sided"),
        exact = NULL)

figure <- ggplot(p.data, aes(x=Age)) + geom_density(aes(group=sample, fill=sample) , alpha=0.9)+
  scale_fill_manual("",  values=colours) + 
  xlab("Age") + ylab("Density") +  theme(legend.position="bottom")

ggsave(figure, filename = "figures/figure_a1.png", device = "png", width = 7, height = 4)

## Figure A2

plot.df<-p.data[, c("DictGive", "sample")]

lab<-plot.df$DictGive[plot.df$sample=="Lab"]
online.lab<-plot.df$DictGive[plot.df$sample=="Online Lab"]
online.panel<-plot.df$DictGive[plot.df$sample=="CESS Online UK"]
Mturk<-plot.df$DictGive[plot.df$sample=="Mturk"]


ks.test(lab, online.lab ,
        alternative = c("two.sided"),
        exact = NULL)

ks.test(online.panel, Mturk,
        alternative = c("two.sided"),
        exact = NULL)


#plot.df$DictGive[plot.df$DictGive==-1]<-NA
plot.df$DictGive2<-ifelse(plot.df$DictGive==0, "0", 
                          ifelse(plot.df$DictGive>0 & plot.df$DictGive<500, ">0 & <500", 
                                 ifelse(plot.df$DictGive==500, "500",  ">500")))
tbl<-prop.table(table(plot.df),2)

plot.df<-as.data.frame(tbl)
plot.df$DictGive2 <- factor(plot.df$DictGive2, levels = c("0", ">0 & <500", "500",  ">500" ))

figure <- ggplot(plot.df, aes(x = DictGive2, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("Offers in Dictator Game") + 
  scale_fill_manual("",  values=colours) + 
  ylim(0, 100) + ylab("Percent")+  theme(legend.position="bottom") 

ggsave(figure, filename = "figures/figure_a2.png", device = "png",  width = 8, height = 5)

## Figure A3

plot.df<-p.data[, c("risk.pref.normal", "sample")]
tbl<-prop.table(table(plot.df),2)

plot.df<-as.data.frame(tbl)

figure <- ggplot(plot.df, aes(x = risk.pref.normal, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("Risk Preferences") + 
  scale_fill_manual("",  values=colours) + 
  ylim(0, 40) + ylab("Percent")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 

ggsave(figure, filename = "figures/figure_a3.png", device = "png",  width = 7, height = 7)

## Figure A4

figure <- ggplot(p.data, aes(x=prelimGain)) + geom_density(aes(group=sample, fill=sample), alpha=0.7)+
  scale_fill_manual("",  values=colours) + 
  xlab("Preliminary Gains") + ylab("Density")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 

ggsave(figure, filename = "figures/figure_a4.png", device = "png",  width = 7, height = 4)


pairwise.t.test(p.data$prelimGain, p.data$sample, p.adjust.method = p.adjust.methods,
                pool.sd = FALSE,
                alternative = c("two.sided"))

pairwise.wilcox.test(p.data$prelimGain, plot.df$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)

## Figure A5

# Duplicate dataframe and order treatment effects
hist <- pred$data
hist <- hist[order(hist$Treatment.effect),]
hist$i <- c(1:nrow(hist))

# Convert covariates to binary factors
hist$sample <- ifelse(hist$onlinelab == 1, "Online Lab", ifelse(hist$mturk == 1, "Mturk", ifelse(hist$cess == 1, "CESS Online UK", "Lab")))
hist$onlinelab <- NULL
hist$mturk <- NULL
hist$cess <- NULL
hist$Gender <- as.factor(ifelse(hist$Gender == 1, "Female","Male"))
hist$sample <- as.factor(hist$sample)

# Recreate Kosuke heterogeneity plot
effectsPlot <- ggplot(hist, aes(x = i, y = Treatment.effect)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$Treatment.effect), color = "blue") +
  labs(y = "Treatment Effect") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,5220))
 
# Create histogram plots, banded age brackets for stacked histogram

modePlot <- ggplot(hist, aes(x=i, fill=sample)) +
  geom_histogram(binwidth = 60, position="stack") +
  theme(legend.position="bottom") +
  scale_fill_manual(name="Online", values = colours) +
  labs(y = "Count") +
  scale_x_continuous(limits = c(0,5220))

## Combine all plots into one chart
figure <- ggarrange(effectsPlot, modePlot,
                    ncol = 1, nrow = 2, heights = c(2,2))

ggsave(figure, filename = "figures/figure_a5.png", device = "png", height = 8, width = 6)

