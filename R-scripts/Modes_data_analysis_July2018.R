# author: Denise Laroze Feb 2017 (deniselaroze@gmail.com)
# amended: Tom Robinson July 2018 (thomas.robinson@politics.ox.ac.uk)


rm(list=ls())
#setwd("~/Dropbox/Ray_Projects/shared_folders/CESS_Aki/Lab_and_Online/APMM2015/Online_Data/R/")
#source("zTree.R")
library(foreign)
library(ggplot2)
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
library(ggpubr)
library(clusterSEs)
library(scales)
library(dplyr)


##--------#---------#---------#---------#---------#---------#---------#---------
## set up the lab, online.mturk, online.uk comparison data
##--------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
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
v<-"July2018"




####################
### Data Management
####################

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

#### Eliminating risk inconsistent preferences
#lab.online.sync<-lab.online.sync[complete.cases(lab.online.sync$risk.pref.normal), ]
#baseline.uk<-baseline.uk[complete.cases(baseline.uk$risk.pref.normal), ]
#mturk.ds<-mturk.ds[complete.cases(mturk.ds$risk.pref.normal), ]
#cess.online.panel<-cess.online.panel[complete.cases(cess.online.panel$risk.pref.normal), ]
#cess.online.stgo<-cess.online.stgo[complete.cases(cess.online.stgo$risk.pref.normal), ]



###################################################################
### Eliminating observations with null risk and integrity scores
### caused by the way the online experiment is coded
##########################################################
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


###############################
## change the unit of variables
###############################
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


#################################
#### Figures - Prep
##################################
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




##########
##################################################################
####                    Descriptive statistics
##################################################################
##########
###################################
### DS: Consistency in risk preferences
###################################
prop.table(table(baseline.uk$Consistent ))
prop.table(table(lab.online.sync$Consistent))
prop.table(table(mturk.ds$Consistent ))
prop.table(table(cess.online.panel$Consistent ))
#prop.table(table(cess.online.stgo$Consistent ))


################################
# DS: Treatments and subject summary
#################################
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


##############################
# DS: Mean Give in dictator game
##############################

ddply(p.data, ~ sample, summarize, 
      m.report = mean(DictGive, na.rm=T)
)

lab<-c( baseline.uk$DictGive, lab.online.sync$DictGive)
others<-c(mturk.ds$DictGive, cess.online.panel$DictGive)

### Students v non-students
wilcox.test(lab, others,
            alternative = "two.sided",conf.level = 0.95)

t.test(lab, others, alternative ="two.sided", conf.level = 0.95)


### General comparisons
pairwise.t.test(p.data$DictGive, p.data$sample, # p.adjust.method = p.adjust.methods,
                pool.sd = FALSE, paired = F,
                alternative = c("two.sided"))

pairwise.wilcox.test(p.data$DictGive, p.data$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)


##########################
### DS: Premilinary Gains
##########################

ddply(p.data, ~ sample, summarize, 
      m.prelimgain = mean(prelimGain, na.rm=T),
      m.correct.ret= mean(ncorrectret, na.rm=T)
)

############################################################
####                         Data Analysis
############################################################

##############################
## Table 3 in the paper
##############################

# Scaling function for 0-1
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

## Output
models <- c( "Lab", "Online Lab", "Online UK",  "Mturk")
covariates <- c( "Ability Rank" , "20\\% Deduction","30\\% Deduction","No Audit","Age","Gender (1 $=$ Female)","Constant")

#Cluster-robust table
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

##############################
## Table 4 in the paper
##############################

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


##############################
## Table 5 in the paper
##############################
set.seed(89)
df <- p.data

# Assign treatment value to subjects based on RET performance
df$treat.het <- ifelse(is.na(df$ncorrectret),NA,ifelse(df$ncorrectret > mean(df$ncorrectret, na.rm=T),1,0))

# Add online and student dummy variables
df$online <- ifelse(df$sample == "Online Lab" | df$sample == "Cess Online UK" | df$sample == "Mturk",1,0)
df$student <- ifelse(df$sample == "Online Lab" | df$sample == "Lab",1,0)

# Recode gender 
df$Gender <- ifelse(df$Gender == "F",1,0)

##################################################################################################
## Basic model with no additional covariates as controls                                        ##
##################################################################################################

## Not run:
# Run to find LASSO parameters (see FindIt help for more info.) 
# F1 <- FindIt(model.treat = report.rate ~ treat.het,
#              model.main = ~ student + online,
#              model.int = ~ student + online,
#              data = df,
#              type = "continuous",
#              treat.type = "single")

# # Use LASSO parameters from above to find best heterogeneous effects model
# F1 <- FindIt(model.treat = report.rate ~ treat.het,
#              model.main = ~ student + online,
#              model.int = ~ student + online,
#              data = df,
#              type = "continuous",
#              treat.type = "single",
#              search.lambdas = FALSE,
#              lambdas = c(-10.250,-10.245))
# 
# summary(F1)
# pred1 <- predict(F1)
# plot(pred1)
# 
# # Generate distinct predicted treatment effects matrix
# te <- distinct(pred1$data, Treatment.effect, treatment, student, online)
# te[te$treatment == 1,]

##################################################################################################
## Model with additional covariates as controls                                                 ##
##################################################################################################

## Not run
# Run to get LASSO parameters
# F2 <- FindIt(model.treat = report.rate ~ treat.het,
#              model.main = ~ student + online + Age + Gender,
#              model.int = ~ student + online,
#              data = df,
#              type = "continuous",
#              treat.type = "single")
# 
# 
# F2 <- FindIt(model.treat = report.rate ~ treat.het,
#              model.main = ~ student + online + Age + Gender,
#              model.int = ~ student + online,
#              data = df,
#              type = "continuous",
#              treat.type = "single",
#              search.lambdas = FALSE,
#              lambdas = c(-8.624,-8.619))
# summary(F2)
# 
# pred2  <-  predict(F2)
# plot(pred2)
# 
# te2 <- distinct(pred2$data, Treatment.effect, treatment, student, online)
# unique(te2[te2$treatment == 1,])

##################################################################################################
## Model with Age (continuous) and Gender (binary) treatment interactions included              ##
##################################################################################################

## Not run:
# Run to get LASSO parameters
# F3 <- FindIt(model.treat = report.rate ~ treat.het,
#              model.main = ~ student + online + Age + Gender,
#              model.int = ~ student + online + Age + Gender,
#              data = df,
#              type = "continuous",
#              treat.type = "single")


F3 <- FindIt(model.treat = report.rate ~ treat.het,
             model.main = ~ student + online + Age + Gender,
             model.int = ~ student + online + Age + Gender,
             data = df,
             type = "continuous",
             treat.type = "single",
             search.lambdas = FALSE,
             lambdas = c(-7.0025,-6.9975))
summary(F3)

pred3  <-  predict(F3)

te3 <- distinct(pred3$data, Treatment.effect, treatment, student, online, Age, Gender)
unique(te3[te3$treatment == 1,])



##################################################################################################
##                                                                                              ##
## 2. Bayesian Additive Regression Trees (BART) Estimation                                      ##
##    Hugh A. Chipman, Edward I. George and Robert E. McCulloch (2010)                          ##
##                                                                                              ##
##################################################################################################
set.seed(89)

# Refresh data
df <- p.data
df$treat.het <- ifelse(is.na(df$ncorrectret),NA,ifelse(df$ncorrectret > mean(df$ncorrectret, na.rm=T),1,0))
# Add online and student dummy variables
df$online <- ifelse(df$sample == "Online Lab" | df$sample == "Cess Online UK" | df$sample == "Mturk",1,0)
df$student <- ifelse(df$sample == "Online Lab" | df$sample == "Lab",1,0)
df$Gender <- ifelse(df$Gender == "F",1,0)

# Define variabels incl. outcome as column 1
vars <- c("report.rate","treat.het","student","online","Age","Gender")

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









####
###############################################################
####                          Figures
###############################################################


colours<-c("Grey10", "Grey30", "Grey60" , "Grey80", "Grey90")

####################
###### Gender figure
#####################

plot.df<-p.data[,c("Gender", "sample")]
tbl<-prop.table(table(plot.df),2)

plot.df<-as.data.frame(tbl)
#plot.df$Gender2[plot.df$Gender==1]<-"Male"
#plot.df$Gender2[plot.df$Gender==2]<-"Female"
plot.df$Gender2[plot.df$Gender=="M"]<-"Male"
plot.df$Gender2[plot.df$Gender=="F"]<-"Female"

ggplot(plot.df, aes(x = Gender2, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("") + 
  scale_fill_manual("", values=colours) + 
  ylim(0, 100) + ylab("Percent")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 


ggsave(paste0("comparative_gender", v, ".pdf"), path=fig.path,  width = 7, height = 4)


##########################
### Hist amount given DG
##########################

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

ggplot(plot.df, aes(x = DictGive2, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("Offers in Dictator Game") + 
  scale_fill_manual("",  values=colours) + 
  ylim(0, 100) + ylab("Percent")+  theme(legend.position="bottom") 
ggsave(paste0("comparative_hist_offers_DG", v, ".pdf"), path=fig.path,  width = 8, height = 5)








######################
### Density age
######################


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

ggplot(p.data, aes(x=Age)) + geom_density(aes(group=sample, fill=sample) , alpha=0.9)+
  scale_fill_manual("",  values=colours) + 
  xlab("Age") + ylab("Density") +  theme(legend.position="bottom")

ggsave(paste0("comparative_density_age", v, ".pdf"), path=fig.path,  width = 7, height = 4)


######################
### Density Integrity
######################

ddply(p.data, ~ sample, summarize, 
      m.report = mean(total.integrity.normal, na.rm=T)
)


lab<-p.data$total.integrity.normal[p.data$sample=="Lab"]
online.lab<-p.data$total.integrity.normal[p.data$sample=="Online Lab"]
online.panel<-p.data$total.integrity.normal[p.data$sample=="CESS Online UK"]
Mturk<-p.data$total.integrity.normal[p.data$sample=="Mturk"]
student<-c(lab, online.lab)
non.student<-c(Mturk, online.panel)


ks.test(lab, online.lab ,
        alternative = c("two.sided"),
        exact = NULL)

ks.test(online.panel, Mturk,
        alternative = c("two.sided"),
        exact = NULL)

ks.test(student, non.student,
        alternative = c("two.sided"),
        exact = NULL)


pairwise.t.test(p.data$total.integrity.normal, p.data$sample, p.adjust.method = p.adjust.methods,
                pool.sd = FALSE,
                alternative = c("two.sided"))

pairwise.wilcox.test(p.data$total.integrity.normal, p.data$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)



ggplot(p.data, aes(x=total.integrity.normal)) + geom_density(aes(group=sample, fill=sample), alpha=0.85)+
  scale_fill_manual("",  values=colours) + 
  xlab("Integrity Score") + ylab("Density")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 
ggsave(paste0("comparative_density_integrity", v, ".pdf"), path=fig.path,  width = 7, height = 4)






##########################
### Hist Risk Preferences
##########################
plot.df<-p.data[, c("risk.pref.normal", "sample")]
tbl<-prop.table(table(plot.df),2)

plot.df<-as.data.frame(tbl)

ggplot(plot.df, aes(x = risk.pref.normal, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("Risk Preferences") + 
  scale_fill_manual("",  values=colours) + 
  ylim(0, 40) + ylab("Percent")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 

ggsave(paste0("comparative_hist_safe_choices", v, ".pdf"), path=fig.path,  width = 7, height = 7)



### T-test differences
plot.df<-p.data[, c("risk.pref.normal", "sample")]
pairwise.t.test(plot.df$risk.pref.normal, plot.df$sample, p.adjust.method = p.adjust.methods,
                paired = FALSE,
                alternative = c("two.sided"))

pairwise.wilcox.test(plot.df$risk.pref.normal, plot.df$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)




#########################
### Density Prelim Gains
#########################

ggplot(p.data, aes(x=prelimGain)) + geom_density(aes(group=sample, fill=sample), alpha=0.7)+
  scale_fill_manual("",  values=colours) + 
  xlab("Preliminary Gains") + ylab("Density")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 

ggsave(paste0("comparative_density_prelimGain", v, ".pdf"), path=fig.path,  width = 7, height = 4)


pairwise.t.test(p.data$prelimGain, p.data$sample, p.adjust.method = p.adjust.methods,
                pool.sd = FALSE,
                alternative = c("two.sided"))

pairwise.wilcox.test(p.data$prelimGain, plot.df$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)


#############################
### Hist Report Rate by audit
##############################

######
p.data$report.rate.r<-round(p.data$report.rate, 1)

plot.df<-p.data[ p.data$treat==1 , c("report.rate.r", "sample")] # With Audit
tbl<-prop.table(table(plot.df),2)

plot.df<-as.data.frame(tbl)
plot.df$treat<-"With Audit"

plot.df2<-p.data[ p.data$treat==2 , c("report.rate.r", "sample")] # Without Audit
tbl<-prop.table(table(plot.df2),2)

plot.df2<-as.data.frame(tbl)
plot.df2$treat<-"Without Audit"

plot.df<-rbind(plot.df, plot.df2)
rm(plot.df2)

p1<-ggplot(plot.df, aes(x = report.rate.r, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("Report Rate") + 
  scale_fill_manual("", values=colours) + 
  ylim(0, 80) + ylab("Percent")+
  facet_wrap( ~  treat ) + theme(legend.position="bottom")
p1

ggsave(paste0("comparative_hist_report_rate", v, ".pdf"), path=fig.path,  width = 12, height = 8)


#################################### 
### Cheat types by sample and audit
####################################

# With Audit
cdata <- ddply(p.data[p.data$treat==1,], c("muID", "sample" ), summarise,
               mean.report.rate = mean(report.rate, na.rm=T),
               cheat_pattern = if (mean.report.rate=="0") "Always declare 0%" 
               else if (mean.report.rate=="1") "Always declare 100%" 
               else if ("1" %in% report.rate) "Sometimes Cheat" else "Always Cheat")

plot.data<-prop.table(table(cdata$sample, cdata$cheat_pattern), 1)

plot.data<-as.data.frame(plot.data)
names(plot.data)[names(plot.data) == "Var1"] <- "Sample"
names(plot.data)[names(plot.data) == "Var2"] <- "cheat_type"

plot.data$cheat_type <- factor(plot.data$cheat_type, levels = c("Always declare 0%", "Always Cheat", "Sometimes Cheat" , "Always declare 100%"))
plot.data$treat<-"With Audit"

#Without audit

cdata <- ddply(p.data[p.data$treat==2,], c("muID", "sample" ), summarise,
               mean.report.rate = mean(report.rate, na.rm=T),
               cheat_pattern = if (mean.report.rate=="0") "Always declare 0%" 
               else if (mean.report.rate=="1") "Always declare 100%" 
               else if ("1" %in% report.rate) "Sometimes Cheat" else "Always Cheat")

plot.data2<-prop.table(table(cdata$sample, cdata$cheat_pattern), 1)

plot.data2<-as.data.frame(plot.data2)
names(plot.data2)[names(plot.data2) == "Var1"] <- "Sample"
names(plot.data2)[names(plot.data2) == "Var2"] <- "cheat_type"

plot.data2$cheat_type <- factor(plot.data$cheat_type, levels = c("Always declare 0%", "Always Cheat", "Sometimes Cheat" , "Always declare 100%"))
plot.data2$treat<-"Without Audit"

plot.data<-rbind(plot.data, plot.data2)
rm(plot.data2)

ggplot(plot.data, aes(x = Sample, y = Freq, fill = cheat_type)) + geom_bar(stat = "identity", position = "dodge") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  scale_fill_manual(values = c("grey10","grey30", "grey50", "grey80"),guide = guide_legend(title = "")) +
  xlab("") + theme(legend.position="bottom", axis.text=element_text(size=10, angle = 45, hjust = 1) ) +
  facet_wrap( ~  treat)

ggsave(paste0("comparative_4types_cheaters", v, ".pdf"), path=fig.path,  width = 10, height = 6)


#################################### 
### FindIT plot
####################################

## Stacked density plot
library(ggplot2)
library(ggpubr)

# Duplicate dataframe and order treatment effects
hist <- pred3$data
hist <- hist[order(hist$Treatment.effect),]
hist$i <- c(1:nrow(hist))

# Convert covariates to binary factors
hist$Gender <- as.factor(ifelse(hist$Gender == 1, "Female","Male"))
hist$online <- as.factor(ifelse(hist$online == 1,"Online","Lab"))
hist$student <- as.factor(ifelse(hist$student == 1,"Student","Non-student"))

# Recreate Kosuke heterogeneity plot
effectsPlot <- ggplot(hist, aes(x = i, y = Treatment.effect)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = pred3$ATE, color = "blue") +
  labs(y = "Treatment Effect") +
  theme_minimal()

## Create histogram plots
# Banded age brackets for stacked histogram
hist$AgeF <- ifelse(hist$Age < 30,"< 30",ifelse(hist$Age <50,"30-49",ifelse(hist$Age<70,"50-69","70+")))
hist$AgeF <- factor(hist$AgeF, levels = c("70+","50-69","30-49","< 30"))

agePlot <- ggplot(hist, aes(x=i, fill = AgeF)) +
  geom_histogram(binwidth = 400, position="stack") +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Age") +
  labs(y = "Count")

genderPlot <- ggplot(hist, aes(x=i, fill=Gender)) +
  geom_histogram(binwidth = 400, position="stack") +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Gender") +
  labs(y = "Count")

studentPlot <- ggplot(hist, aes(x=i, fill=student)) +
  geom_histogram(binwidth = 400, position="stack") +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Student") +
  labs(y = "Count")

onlinePlot <- ggplot(hist, aes(x=i, fill=online)) +
  geom_histogram(binwidth = 400, position="stack") +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Online") +
  labs(y = "Count")

## Combine all plots into one chart5
figure <- ggarrange(effectsPlot, agePlot, genderPlot, studentPlot, onlinePlot,
                    ncol = 1, nrow = 5, heights = c(2,1.3,1.3,1.3,1.3))
plot(figure)

ggsave(figure, filename = "CombinedPlot.png", device = "png", height = 8, width = 6)


#################################### 
### BART plot
####################################

# Duplicate dataframe and order treatment effects
hist <- CATE_df

# Convert covariates to binary factors
hist$Gender <- as.factor(ifelse(hist$Gender == 1, "Female","Male"))
hist$online <- as.factor(ifelse(hist$online == 1,"Online","Lab"))
hist$student <- as.factor(ifelse(hist$student == 1,"Student","Non-student"))

# CATE Heterogeneity plot

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal()

## Create histogram plots
# Banded age brackets for stacked histogram
hist$AgeF <- ifelse(hist$Age < 30,"< 30",ifelse(hist$Age <50,"30-49",ifelse(hist$Age<70,"50-69","70+")))
hist$AgeF <- factor(hist$AgeF, levels = c("70+","50-69","30-49","< 30"))

agePlot <- ggplot(hist, aes(x=id, fill = AgeF)) +
  geom_histogram(binwidth = 400, position="stack") +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Age") +
  labs(y = "Count")

genderPlot <- ggplot(hist, aes(x=id, fill=Gender)) +
  geom_histogram(binwidth = 400, position="stack") +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Gender") +
  labs(y = "Count")

studentPlot <- ggplot(hist, aes(x=id, fill=student)) +
  geom_histogram(binwidth = 400, position="stack") +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Student") +
  labs(y = "Count")

onlinePlot <- ggplot(hist, aes(x=id, fill=online)) +
  geom_histogram(binwidth = 400, position="stack") +
  theme(legend.position="bottom") +
  scale_fill_discrete(name="Online") +
  labs(y = "Count")

# Combine all plots into one chart
figure <- ggarrange(effectsPlot, agePlot, genderPlot, studentPlot, onlinePlot,
                    ncol = 1, nrow = 5, heights = c(2,1.3,1.3,1.3,1.3))

ggsave(figure, filename = "CombinedPlot_BART.png", device = "png", height = 8, width = 6)

## Not run:
plot(figure)

