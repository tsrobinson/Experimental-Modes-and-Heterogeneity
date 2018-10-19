# Modes and Heterogeneity - Replication File

# Author: Denise Laroze Feb 2017 (deniselaroze@gmail.com)
# Contributor: Tom Robinson October 2018 (thomas.robinson@politics.ox.ac.uk)

#### Pre-requisites ####

rm(list=ls())
library(foreign)
library(ggplot2)
library(ggpubr)
theme_set(theme_bw())
library(stargazer)
#library(reshape2)
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
library(scales)
library(lme4)
library(sjstats)
library(dplyr)
library(ltm)
library(wesanderson)

source("dist_diff.R")

#### Data Management ####

## Load files

rm(list=ls())
setwd("/Users/tomrobinson/OneDrive/CESS/Heterogeneous")
#setwd("C:/Users/Denise Laroze Prehn/Dropbox/CESS-Santiago/Archive/Modes/Tax Cheating Qualtrics Online Experiment")
#setwd("C:/Users/Denise Laroze P/Dropbox/CESS-Santiago/Archive/Modes/Tax Cheating Qualtrics Online Experiment")

mturk.ds<-read.csv("data/Mturk_DS_Sept2017.csv") #

cess.online.panel <- read.csv("data/CESS_Panel_DS_Feb2018.csv")
cess.online.panel<-cess.online.panel[cess.online.panel$correct>0, ]

cess.online.stgo <- read.csv("data/CESS_Panel_DS_Stgo_2017.csv")

lab.online.sync <- read.csv("data/lab_online_sync_edited.csv")

baseline.uk<-read.csv("data/baseline_uk.csv")
baseline.uk<-baseline.uk[baseline.uk$auditrate<30, ] # Only sessions with 0 and 20% audit

fig.path<-"R-Script/Figures"
v<-"October2018"

## Data cleaning

#Cleaning up round number in lab version of the experiment
baseline.uk$round<-ifelse(baseline.uk$auditrate>0, baseline.uk$period+10, baseline.uk$period )

# Eliminating missing values
lab.online.sync$DictGive[lab.online.sync$DictGive==-1]<-NA

# Editing Age from logical to numeric

mturk.ds$age2<-mturk.ds$age
mturk.ds$age2[mturk.ds$age=="false"]<-NA
mturk.ds$Age<-as.numeric(levels(mturk.ds$age2))[mturk.ds$age2]

cess.online.stgo$age2<-cess.online.stgo$age
cess.online.stgo$age2[cess.online.stgo$age=="false"]<-NA
cess.online.stgo$Age<-as.numeric(levels(cess.online.stgo$age2))[cess.online.stgo$age2]

# Indentifying and Eliminating non-consistent risk preferences
# Eliminating observations null risk and integrity ofvservations 
# caused by the way the online experiment is coded
cess.online.panel$risk.pref[cess.online.panel$risk.pref==0]<-NA
mturk.ds$risk.pref[mturk.ds$risk.pref==0]<-NA
lab.online.sync$risk.pref[lab.online.sync$risk.pref==0]<-NA

cess.online.panel$total.integrity[cess.online.panel$total.integrity==0]<-NA
mturk.ds$total.integrity[mturk.ds$total.integrity==0]<-NA
lab.online.sync$total.integrity[lab.online.sync$total.integrity==0]<-NA

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

#online.uk<-consist.risk(online.uk, "Risk_")
lab.online.sync<-consist.risk(lab.online.sync, "risk")
mturk.ds<-consist.risk(mturk.ds, "risk")
cess.online.panel<-consist.risk(cess.online.panel, "risk")
cess.online.stgo<-consist.risk(cess.online.stgo, "risk")

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

## Eliminating risk inconsistent preferences
#lab.online.sync<-lab.online.sync[complete.cases(lab.online.sync$risk.pref.normal), ]
#baseline.uk<-baseline.uk[complete.cases(baseline.uk$risk.pref.normal), ]
#mturk.ds<-mturk.ds[complete.cases(mturk.ds$risk.pref.normal), ]
#cess.online.panel<-cess.online.panel[complete.cases(cess.online.panel$risk.pref.normal), ]
#cess.online.stgo<-cess.online.stgo[complete.cases(cess.online.stgo$risk.pref.normal), ]


## Eliminating observations with null risk and integrity scores
## caused by the way the online experiment is coded

#lab.online.sync<-lab.online.sync[complete.cases(lab.online.sync$risk.pref.normal), ]
#baseline.uk<-baseline.uk[complete.cases(baseline.uk$risk.pref.normal), ]
#mturk.ds<-mturk.ds[complete.cases(mturk.ds$risk.pref.normal), ]
#cess.online.panel<-cess.online.panel[complete.cases(cess.online.panel$risk.pref.normal), ]
#cess.online.stgo<-cess.online.stgo[complete.cases(cess.online.stgo$risk.pref.normal), ]

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

cess.online.stgo$DictGive.normal <- cess.online.stgo$DictGive/1000
cess.online.stgo$total.integrity.normal <- (cess.online.stgo$total.integrity)/40
cess.online.stgo$risk.pref.normal <- (cess.online.stgo$risk.pref.consist)/10


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

#baseline.uk$taxBracket[baseline.uk$taxrate==10]<-3
#baseline.uk$taxBracket[baseline.uk$taxrate==20]<-2
#baseline.uk$taxBracket[baseline.uk$taxrate==30]<-1

baseline.uk$Gender_lab[baseline.uk$gender==0]<-"F"
baseline.uk$Gender_lab[baseline.uk$gender==1]<-"M"

### Number of correct responses
lab.online.sync$ncorrectret <- lab.online.sync$correct
mturk.ds$ncorrectret<-mturk.ds$correct
cess.online.panel$ncorrectret<-cess.online.panel$correct
cess.online.stgo$ncorrectret<-cess.online.stgo$correct

## Figures - Prep
baseline.uk$muID<-paste0("baseline", baseline.uk$subj_id)
names(baseline.uk)[names(baseline.uk)=="age_subject"] <- "Age"
names(baseline.uk)[names(baseline.uk)=="safechoices"] <- "risk.pref"
names(baseline.uk)[names(baseline.uk)=="profitret"] <- "prelimGain"
names(baseline.uk)[names(baseline.uk)=="offerdg"] <- "DictGive"
names(baseline.uk)[names(baseline.uk)=="Gender_lab"] <- "Gender"

names(lab.online.sync)[names(lab.online.sync)=="age"] <- "Age"
names(lab.online.sync)[names(lab.online.sync)=="gender"] <- "Gender"
names(lab.online.sync)[names(lab.online.sync)=="taxRate"] <- "taxrate"

names(mturk.ds)[names(mturk.ds)=="gender"] <- "Gender"
names(mturk.ds)[names(mturk.ds)=="taxRate"] <- "taxrate"

names(cess.online.panel)[names(cess.online.panel)=="age"] <- "Age"
names(cess.online.panel)[names(cess.online.panel)=="gender"] <- "Gender"
names(cess.online.panel)[names(cess.online.panel)=="taxRate"] <- "taxrate"

names(cess.online.stgo)[names(cess.online.stgo)=="gender"] <- "Gender"
names(cess.online.stgo)[names(cess.online.stgo)=="taxRate"] <- "taxrate"

# Call consistency analysis here
# NB: WON'T WORK UNTIL THIS IS CALLED, AS RISK_SWI... INTEG... VARS NOT IN ORIGINAL DATA


vars<-c( "muID", "ncorrectret" ,"Gender", "Age", "DictGive" ,"DictGive.normal", "total.integrity", "total.integrity.normal", 
         "risk.pref.normal", "risk.pref", "prelimGain", "report.rate", "treat", "taxrate", "round"
)
o.sync<-lab.online.sync[, vars]
o.sync$sample<-"Online Lab"
b.s<-baseline.uk[, vars]
b.s$sample<-"Lab"
mt.s<-mturk.ds[, vars]
mt.s$sample<-"Mturk"

cp.s<-cess.online.panel[, vars]
cp.s$sample<-"CESS Online UK"

#cps.s<-cess.online.stgo[, vars]
#cps.s$sample<-"CESS Online Stgo"

p.data<-rbind(o.sync, b.s, mt.s, cp.s)

rm(o.sync, b.s, mt.s, cp.s)


#p.data$study<-"BD"
#p.data$study[p.data$sample %in% c("Online Lab-DS", "Lab-DS", "Mturk-DS"  )]<-"DS"

## Only cases with rational risk preferences
#p.data<-p.data[complete.cases(p.data$risk.pref.normal), ]

## Unique Subject Ids
p.data$muID<-paste0(p.data$sample, p.data$muID)

#### Descriptive Stats ####

## DS: Consistency in risk preferences
prop.table(table(baseline.uk$Consistent ))
prop.table(table(lab.online.sync$Consistent))
prop.table(table(mturk.ds$Consistent ))
prop.table(table(cess.online.panel$Consistent ))
#prop.table(table(cess.online.stgo$Consistent ))



## DS: Treatments and subject summary
tbl<-ddply(p.data, ~ sample, summarize, 
           m.report = mean(report.rate, na.rm=T),
           subj.n = length(unique(muID))
)

names(tbl)<-c("Sample", "Mean Report Rate", "Number Subjects")
tbl<-xtable(tbl, caption="Treatment Summaries", label="table:sum")
print(tbl)
print(tbl, type = getOption("xtable.type", "latex"), file = "R-Script/Tables/treatsum.tex")

table(baseline.uk$session)
length(unique(baseline.uk$session))


## DS: Mean Give in dictator game

ddply(p.data, ~ sample, summarize, 
      m.report = mean(DictGive, na.rm=T)
)

lab<-c( baseline.uk$DictGive, lab.online.sync$DictGive)
others<-c(mturk.ds$DictGive, cess.online.panel$DictGive)

# Students v non-students
wilcox.test(lab, others,
            alternative = "two.sided",conf.level = 0.95)

t.test(lab, others, alternative ="two.sided", conf.level = 0.95)


# General comparisons
pairwise.t.test(p.data$DictGive, p.data$sample, # p.adjust.method = p.adjust.methods,
                pool.sd = FALSE, paired = F,
                alternative = c("two.sided"))

pairwise.wilcox.test(p.data$DictGive, p.data$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)


## DS: Premilinary Gains

ddply(p.data, ~ sample, summarize, 
      m.prelimgain = mean(prelimGain, na.rm=T),
      m.correct.ret= mean(ncorrectret, na.rm=T)
)


#### Tables ####

## Table 1 - GLM model with clustered s.e's

redist.fun <- function(x){(x-min(x, na.rm = T))/diff(range(x, na.rm = T))}

# Calculatue normalized ability ranking
abilityCalc <- function(df) {
  
  df$ability <- as.numeric(lapply(df$muID, function(x) mean(df[df$muID == x,]$ncorrectret, na.rm=T)))
  abilityDF <- group_by(df,muID)
  newDF <- summarise(abilityDF,
                     ability = mean(ability))
  newDF$rank <- rank(newDF$ability, na.last = "keep")
  df$rank <- as.numeric(lapply(df$muID, function(x) mean(newDF[newDF$muID == x,]$rank, na.rm=T)))
  df$rank <- redist.fun(df$rank)
  
  # Format gender
  df$Gender <- ifelse(df$Gender == "F",1,0)
  return(df)
}

lab.online.sync.ab <- abilityCalc(lab.online.sync[,c("muID","report.rate","ncorrectret","taxrate","treat","Age","Gender")])
baseline.uk.ab <- abilityCalc(baseline.uk[,c("muID","report.rate","ncorrectret","taxrate","treat","Age","Gender")])
mturk.ds.ab <- abilityCalc(mturk.ds[,c("muID","report.rate","ncorrectret","taxrate","treat","Age","Gender")])
cess.online.panel.ab <- abilityCalc(cess.online.panel[,c("muID","report.rate","ncorrectret","taxrate","treat","Age","Gender")])

# Models
model.1.lab.online.sync <- glm(report.rate ~ rank + factor(taxrate) +factor(treat) + Age + factor(Gender), data=lab.online.sync.ab)
model.1.baseline <- glm(report.rate ~ rank + factor(taxrate) +factor(treat) + Age + factor(Gender), data=baseline.uk.ab)
model.1.mt.ds <- glm(report.rate ~ rank + factor(taxrate) +factor(treat) + Age + factor(Gender), data=mturk.ds.ab)
model.1.cp.ds <- glm(report.rate ~ rank + factor(taxrate) +factor(treat) + Age + factor(Gender), data=cess.online.panel.ab)

# Basic clustering

model.1.lab.online.sync.cluster <- coeftest(model.1.lab.online.sync, vcov=vcovHC(model.1.lab.online.sync, cluster = "muID", type = "HC0"))
model.1.baseline.cluster <- coeftest(model.1.baseline, vcov=vcovHC(model.1.baseline, cluster = "muID", type = "HC0"))
model.1.mt.ds.cluster <- coeftest(model.1.mt.ds, vcov=vcovHC(model.1.mt.ds, cluster = "muID", type = "HC0"))
model.1.cp.ds.cluster <- coeftest(model.1.cp.ds, vcov=vcovHC(model.1.cp.ds, cluster = "muID", type = "HC0"))

# Output
models <- c( "Lab", "Online Lab", "Online UK",  "Mturk")
covariates <- c( "Ability Rank" , "20\\% Deduction","30\\% Deduction","No Audit","Age","Gender (1 $=$ Female)","Constant")

# Cluster-robust table
stargazer(model.1.baseline.cluster, model.1.lab.online.sync.cluster, model.1.cp.ds.cluster, model.1.mt.ds.cluster,
          column.labels = models,
          dep.var.caption = "Mode",
          model.numbers = F,
          covariate.labels = covariates,
          title = "GLM estimation on percent declared",
          label = "table:glm",
          
          se = list(model.1.baseline.cluster[,2],
                    model.1.lab.online.sync.cluster[,2],
                    model.1.cp.ds.cluster[,2],
                    model.1.mt.ds.cluster[,2]),
          out="table.3.tex")

## Table 2/3 - ICCs by mode, for outcome and RET variables

colnames(baseline.uk)[colnames(baseline.uk) == "auditrate"] <- "auditRate"

icc_modes <- function(taxrate, auditrate, y) {
  
  model <- ifelse(y == "report.rate","report.rate ~ (1|muID)","ncorrectret ~ (1|muID)")
  
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
  
  return(c(icc(fit_baseline_y_subset),
           icc(fit_lab.online_y_subset),
           icc(fit_cess.online_y_subset),
           icc(fit_mturk_y_subset)))
}

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

## Bootstrap ICC estimate functions
# Credit: https://stats.stackexchange.com/questions/232252/intraclass-correlation-standard-error/232284

icc_se <- function(data, taxrate,auditrate,var) {
  
  if (auditrate == 0) {
    data <- data[data$taxrate == taxrate & data$auditRate == 0,]
  } else {
    data <- data[data$taxrate == taxrate & data$auditRate != 0,]
  }
  
  model <- ifelse(var == "report.rate","report.rate ~ (1 | muID)", "ncorrectret ~ (1 | muID)")
  
  dist <- c()
  for (i in 1:1000) {
    dummy <- data[sample(nrow(data), replace=TRUE),]
    dist[i] <- icc(lmer(model, data = dummy))
  }
  
  return(sd(dist))
}

# Report rate ICC standard errors
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

# RET ICC standard errors
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

# Build output tables
bracket <- function(vec) {
  for (i in 1:4){
    vec[i] <- paste0("(",round(as.numeric(vec[i]), 3),")")  
  }
  return(vec)
}

baseline_y <- bracket(baseline_y)
lab.online_y <- bracket(lab.online_y)
cess.online_y <- bracket(cess.online_y)
mturk_y <- bracket(mturk_y)

baseline_ret <- bracket(baseline_ret)
lab.online_ret <- bracket(lab.online_ret)
cess.online_ret <- bracket(cess.online_ret)
mturk_ret <- bracket(mturk_ret)

modes <- c("Lab","Lab Online","CESS Onine","MTurk")
y_table <- round(cbind(reportrate_subset_10_0_icc,
                       reportrate_subset_30_0_icc,
                       reportrate_subset_10_a_icc,
                       reportrate_subset_30_a_icc),3)

tax <- c("10%","30%","10%","30%")
audit <- c("No","No","Yes","Yes")

y_table <- rbind(y_table[1,],
                 baseline_y,
                 y_table[2,],
                 lab.online_y,
                 y_table[3,],
                 cess.online_y,
                 y_table[4,],
                 mturk_y)

y_table <- rbind(y_table, tax, audit)

modes <- c("Lab","","Lab Online","","CESS Onine","","MTurk","","Tax Rate","Audited?")

y_table <- cbind(modes,y_table)

colnames(y_table) <- c("Mode","(1)","(2)","(3)","(4)")

# Table 2
print(xtable(y_table, row.names = FALSE, caption = "Comparison of outcome ICCs across modes", label = "tab:icc_report"),
      include.rownames = F, hline.after = c(-1,0,8,10),
      file = "table_icc_reportrate.tex")

ret_table <- round(cbind(ret_subset_10_0_icc,
                         ret_subset_30_0_icc,
                         ret_subset_10_a_icc,
                         ret_subset_30_a_icc),3)

ret_table <- rbind(ret_table[1,],
                   baseline_ret,
                   ret_table[2,],
                   lab.online_ret,
                   ret_table[3,],
                   cess.online_ret,
                   ret_table[4,],
                   mturk_ret)

ret_table <- round(ret_table,3)

ret_table <- rbind(ret_table, tax, audit)

ret_table <- cbind(modes,ret_table)

colnames(ret_table) <- c("Mode","(1)","(2)","(3)","(4)")

# Table 3
print(xtable(ret_table, row.names = FALSE, caption = "Comparison of RET ICCs across modes", label = "tab:icc_ret"),
      include.rownames = FALSE, hline.after = c(-1,0,8,10),
      file = "table_icc_ret.tex")

## Table 4

integrity_measures <- c("integrity1","integrity2","integrity3","integrity4","integrity5","integrity6","integrity7","integrity8","integrity9","integrity10")
integrity_measures_base <- c("publictransport","taxes","drivingfast","moneyfound","lying","accidentaldamage","litter","drivingalcohol","jobapplication","buyingstolen")

cronbach_fun <- function(data, integrity_measures) {
  df <- data[,c(integrity_measures,"muID")]
  df <- unique(df)
  return(cronbach.alpha(df[,integrity_measures], na.rm = T, CI = T))
}

baseline_ca <- cronbach_fun(baseline.uk, integrity_measures_base)
lab.online_ca <- cronbach_fun(lab.online.sync, integrity_measures)
cess.online_ca <- cronbach_fun(cess.online.panel, integrity_measures)
mturk_ca <- cronbach_fun(mturk.ds, integrity_measures)

modes <- c("Lab","Lab Online","CESS Online","MTurk")
alphas <- round(c(baseline_ca$alpha, lab.online_ca$alpha, cess.online_ca$alpha, mturk_ca$alpha),3)
ca_ci <- round(rbind(baseline_ca$ci, lab.online_ca$ci, cess.online_ca$ci, mturk_ca$ci),3)

integrity_table <- cbind(modes,alphas,ca_ci)
colnames(integrity_table) <- c("Mode","Cronbach's Alpha","95% Lower Bound","95% Upper Bound")

# Output
print(xtable(integrity_table, row.names = FALSE, caption = "LTM Cronbach's alphas for integrity responses", label = "tab:integrity_alpha"),
      include.rownames = FALSE, hline.after = c(-1,0,4),
      file = "table_integrity_alpha.tex")

#### Figures ####

## Figure 1 - BART plot

set.seed(89)

df <- p.data
df$treat.het <- ifelse(is.na(df$taxrate),NA,ifelse(df$taxrate > 10,1,0))
df$ability <- ifelse(is.na(df$ncorrectret),NA,ifelse(df$ncorrectret > mean(df$ncorrectret, na.rm=T),1,0))
df$Gender <- ifelse(df$Gender == "F",1,0)
df$sample <- as.factor(df$sample)

# Define variables incl. outcome as column 1
vars <- c("report.rate","treat.het","ability","sample","Age","Gender")

df <- df[,vars]
df <- df[complete.cases(df),]

y <- df$report.rate
train <- df[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates
CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
          bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)
covars <- rbind(train[train$treat.het == 1,c(2:5)], test[test$treat.het==1,c(2:5)])

CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))

# Mode reduced plot with deduction rate as treatment
hist <- CATE_df

# CATE Heterogeneity plot

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
figure <- ggarrange(effectsPlot, modePlot,
                    ncol = 1, nrow = 2, heights = c(2,2))

# ggsave(figure, filename = "BART_tax_plot_reduced.png", device = "png", height = 8, width = 6)

## Figure 2

df <- p.data[p.data$treat == 2,]

cdata <- ddply(df, c("sample"), summarise,
               mean.report.rate = mean(report.rate, na.rm=T))

plot_df <- cdata
plot_df$facet <- "All Modes"
plot_df$age <- "All"

df$age <- ifelse(is.na(df$Age),NA,ifelse(df$Age < 30, "Age < 30", "Age > 30"))

cdata <- ddply(df[df$sample %in% c("CESS Online UK","Mturk"),], 
               c("sample","age"), summarise,
               mean.report.rate = mean(report.rate, na.rm=T))

cdata$facet <- "Online Modes Age Breakdown"

plot_df <- rbind(plot_df,cdata)
plot_df <- plot_df[complete.cases(plot_df),]
plot_df$facet <- factor(plot_df$facet, levels=c("All Modes", "Online Modes Age Breakdown"))
plot_df$sample <- factor(plot_df$sample, levels=c("Lab","Online Lab", "CESS Online UK", "Mturk"))

figure <- ggplot(plot_df, aes(x = sample, y=mean.report.rate, fill=age)) +
  facet_grid( ~ facet, space = "free", scales = "free") +
  geom_col(position = "dodge") +
  labs(x = "Age", y = "Mean Report Rate") +
  scale_fill_manual("", values = colours[c(4,2,1)]) +
  # scale_fill_brewer(palette = "Set1") +
  theme(legend.position="bottom")

ggsave("Cheat_rates_modes_and_young.png", device = "png", height = 5, width = 8)



## Figure 3

# Duplicate dataframe for distribution of covariates analysis
dist_df <- CATE_df

dist_df$Gender <- ifelse(dist_df$Gender == 1,"Female",ifelse(dist_df$Gender == 0,"Male",NA))
dist_df$age_bin <- ifelse(dist_df$Age > 29,"Aged 30+",ifelse(dist_df$Age < 30,"Age < 30",NA))
dist_df$ability <- ifelse(dist_df$ability == 1, "High Ability",ifelse(dist_df$ability == 0, "Low Ability", NA))

set.seed(89)
BART_het_test_gender_mode <- distdiff(dist_df, cate_var = "CATE", covar = "Gender", value = "Female")
BART_het_test_ability_mode <- distdiff(dist_df, cate_var = "CATE", covar = "ability", value = "High Ability")
BART_het_test_age_mode <- distdiff(dist_df, cate_var = "CATE", covar = "age_bin", value = "Age < 30")

# Mturk only sample dist. diff
set.seed(89)
mturk_het_test_gender_mode <- distdiff(dist_df[dist_df$sample == "Mturk",], cate_var = "CATE", covar = "Gender", value = "Female")
mturk_het_test_ability_mode <- distdiff(dist_df[dist_df$sample == "Mturk",], cate_var = "CATE", covar = "ability", value = "High Ability")
mturk_het_test_age_mode <- distdiff(dist_df[dist_df$sample == "Mturk",], cate_var = "CATE", covar = "age_bin", value = "Age < 30")

mturk_het_test_age_mode$graph <- mturk_het_test_age_mode$graph + labs(title = "") + scale_fill_manual(values = colours2[c(1:5)])
BART_het_test_age_mode$graph <- BART_het_test_age_mode$graph + labs(title = "") + scale_fill_manual(values = colours2[c(1:5)])

figure <- ggarrange(BART_het_test_age_mode$graph,
                    mturk_het_test_age_mode$graph,
                    ncol = 2, nrow = 1,
                    labels = c("Age (1 = < 30): All subjects","Age (1 = < 30): MTurk subjects"))
# heights = c(2,1.3,1.3,1.3))



figure <- annotate_figure(figure, 
                          bottom = text_grob(paste0("N per bin = ",BART_het_test_age_mode$bin_width," (All); ",
                                                    mturk_het_test_age_mode$bin_width," (Mturk)"),
                                             face = "italic", size = 10))

# Not run:
ggsave(figure, filename = "banded_test_all_vs_mturk.png", device = "png", height = 5, width = 9)

#### Appendix Tables ####

## Table A1
                               
sum.table<-ddply(p.data, c("sample"), summarize,
                 DG="Yes",
                 Risk="Yes",
                 auditrate=as.character(list(unique(auditRate))),
                 taxrate=as.character(list(unique(taxrate))),
                 m.report = mean(report.rate, na.rm=T),
                 n = length(unique(muID))
                 )
sum.table

gender.t<-prop.table(table(p.data$sample, p.data$Gender),1)
gender.t<-as.data.frame(gender.t)

sum.table$pc.female<-gender.t[1:4,3]
sum.table$pc.male<-gender.t[5:8,3]

place<-nrow(sum.table)+1
sum.table[place, "sample"]<-"All"
sum.table[place, "DG"]<-"Yes"
sum.table[place, "Risk"]<-"Yes"
sum.table[place, "auditrate"]<-as.character(list(unique(p.data$auditRate)))
sum.table[place, "taxrate"]<-as.character(list(unique(p.data$taxrate)))
sum.table[place, "m.report"]<-mean(p.data$report.rate, na.rm=T)

gender<-prop.table(table (p.data$Gender))
sum.table[place, "pc.female"]<-gender[1]
sum.table[place, "pc.male"]<-gender[2]

n<-length(unique(p.data$muID))
sum.table[place, "n"]<-n


names(sum.table) <- c('Mode','DG', 'Risk' ,'Audit Rate' ,"Tax Rate", "Report Rate" ,"\\# Subjects", "\\% Female", "\\% Male" )

xt<-xtable(sum.table)
print(xt, type="latex", file=("R-scripts/summary_table.tex"), floating=FALSE, include.rownames=FALSE)

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
addtorow$pos <- list(0, 0)
addtorow$command <- c("& \\multicolumn{4}{c}{\\textbf{Wild}} & \\multicolumn{4}{c}{\\textbf{PCB}}\\\\\n",
                      "& Lab & Online Lab & Online UK & MTurk & Lab & Online Lab & Online UK & MTurk \\\\\n")

tab4 <- xtable(clusterTable, caption = "Wild and PCB clustered p-values", label = "table:cluster")
align(tab4) <- "llcccc|cccc"
print(tab4,include.rownames = FALSE, include.colnames = FALSE, 
      add.to.row = addtorow,
      file = "table.cluster.tex", floating.environment = "table")

## Table A3

set.seed(89)
df <- p.data

# Assign treatment value to subjects based on RET performance
df$treat.het <- df$treat.het <- ifelse(is.na(df$taxrate),NA,ifelse(df$taxrate > 10,1,0))
df$ability <- ifelse(is.na(df$ncorrectret),NA,ifelse(df$ncorrectret > mean(df$ncorrectret, na.rm=T),1,0))
df$Gender <- ifelse(df$Gender == "F",1,0)
df$onlinelab <- ifelse(is.na(df$sample),NA,ifelse(df$sample == "Online Lab",1,0))
df$mturk <- ifelse(is.na(df$sample),NA,ifelse(df$sample == "Mturk",1,0))
df$cess <- ifelse(is.na(df$sample),NA,ifelse(df$sample == "CESS Online UK",1,0))

# First, find tuning parameters
# F1 <- FindIt(model.treat = report.rate ~ treat.het,
#              model.main = ~ onlinelab + mturk + cess + ability + Age + Gender,
#              model.int = ~ onlinelab + mturk + cess + ability + Age + Gender,
#              data = df,
#              type = "continuous",
#              treat.type = "single",
#              search.lambdas = TRUE)

# Use tuning parameters for final model estimate
F1 <- FindIt(model.treat = report.rate ~ treat.het,
             model.main = ~ onlinelab + mturk + cess + ability + Age + Gender,
             model.int = ~ onlinelab + mturk + cess + ability + Age + Gender,
             data = df,
             type = "continuous",
             treat.type = "single",
             search.lambdas = FALSE,
             lambdas = c(-8.253,-8.248))
summary(F1)

pred  <-  predict(F1)

te <- distinct(pred$data, Treatment.effect, treatment, onlinelab, mturk, cess, ability, Age, Gender)
te$sample <- ifelse(te$onlinelab == 1, "Online Lab", ifelse(te$mturk == 1, "Mturk", ifelse(te$cess == 1, "CESS Online UK", "Lab")))
te$onlinelab <- NULL
te$mturk <- NULL
te$cess <- NULL

#Not run:
unique(te[te$treatment == 1,])

#### Appendix Figures ####

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

ggsave(figure, filename = "comparative_density_age.png", device = "png", width = 7, height = 4)

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

ggsave(figure, filename = "comparative_hist_offers_DG.png", device = "png",  width = 8, height = 5)

## Figure A3

plot.df<-p.data[, c("risk.pref.normal", "sample")]
tbl<-prop.table(table(plot.df),2)

plot.df<-as.data.frame(tbl)

figure <- ggplot(plot.df, aes(x = risk.pref.normal, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("Risk Preferences") + 
  scale_fill_manual("",  values=colours) + 
  ylim(0, 40) + ylab("Percent")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 

ggsave(figure, filename = "comparative_hist_safe_choices.png", device = "png",  width = 7, height = 7)

## Figure A4

figure <- ggplot(p.data, aes(x=prelimGain)) + geom_density(aes(group=sample, fill=sample), alpha=0.7)+
  scale_fill_manual("",  values=colours) + 
  xlab("Preliminary Gains") + ylab("Density")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 

ggsave(figure, filename = "comparative_density_prelimGain.png", device = "png",  width = 7, height = 4)


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

## Combine all plots into one chart5
figure <- ggarrange(effectsPlot, modePlot,
                    ncol = 1, nrow = 2, heights = c(2,2))

ggsave(figure, filename = "FindIt_Reduced_Form.png", device = "png", height = 8, width = 6)
