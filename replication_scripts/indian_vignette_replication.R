######################################################################
##                                                                  ## 
##      PROJECT TITLE:    Multi-Modes for Detecting Experimental    ##
##                        Measurement Error                         ##
##                                                                  ## 
##      CODE AUTHOR:      THOMAS S. ROBINSON (APRIL 2019)           ##
##                                                                  ##
##      EMAIL:            thomas.robinson@politics.ox.ac.uk         ##
##                                                                  ##
##      DESCRIPTION:      Indian vignette experiment replication    ##
##                                                                  ##
######################################################################

#### Packages ####
library(plyr)
library(tidyverse)
library(broom)
library(xtable)
library(stargazer)

#### Data formatting ####

# CESS Online data
data_co <- read_csv("data/co_exp.csv") %>%
  
  # Rename questions for consistency
  rename(QTE1_1 = Q510_1,
         QTE2_1 = Q511_1,
         QTE3_1 = Q512_1,
         QTE4_1 = Q513_1) %>%
  select(QTE1_1,QTE2_1,QTE3_1,QTE4_1,
         yob, gender) %>%
  
  # Add labels
  mutate(error_type = ifelse(!is.na(QTE1_1),"Control",
                             ifelse(!is.na(QTE2_1),"Control",
                                    ifelse(!is.na(QTE3_1),"High",
                                           ifelse(!is.na(QTE4_1),"High",NA)))),
         treat = ifelse(!is.na(QTE1_1),"Authentic",
                        ifelse(!is.na(QTE2_1),"Fake",
                               ifelse(!is.na(QTE3_1),"Authentic",
                                      ifelse(!is.na(QTE4_1),"Fake",NA)))),
         response = as.numeric(ifelse(!is.na(QTE1_1),QTE1_1,
                                      ifelse(!is.na(QTE2_1),QTE2_1,
                                             ifelse(!is.na(QTE3_1),QTE3_1,
                                                    ifelse(!is.na(QTE4_1),QTE4_1,NA)))))) %>%
  # Remove empty responses:
  filter(!is.na(response)) %>%
  mutate(response = as.numeric(response))

# MTurk data formatting

data_mturk <- read_csv("data/mturk_exp.csv") %>%
  .[-c(1:2),] %>%
  filter(grepl("01/04/2019",.$StartDate)) %>%
  mutate(error_type = ifelse(!is.na(QTE1_1),"Control",
                                ifelse(!is.na(QTE2_1),"Control",
                                       ifelse(!is.na(QTE3_1),"High",
                                              ifelse(!is.na(QTE4_1),"High",NA)))),
         treat = ifelse(!is.na(QTE1_1),"Authentic",
                            ifelse(!is.na(QTE2_1),"Fake",
                                   ifelse(!is.na(QTE3_1),"Authentic",
                                          ifelse(!is.na(QTE4_1),"Fake",NA)))),
         response = as.numeric(ifelse(!is.na(QTE1_1),QTE1_1,
                                      ifelse(!is.na(QTE2_1),QTE2_1,
                                             ifelse(!is.na(QTE3_1),QTE3_1,
                                                    ifelse(!is.na(QTE4_1),QTE4_1,NA)))))) %>%
  filter(!is.na(response)) %>%
  mutate(response = as.numeric(response),
         yob = as.numeric(Q67),
         gender = Q2) %>%
  select(colnames(data_co))
  

# Incentivised MTurk formatting

data_mturk2 <- read_csv("data/mturk_exp_incentivised.csv") %>%
  .[-c(1:2),] %>%
  filter(Finished == TRUE) %>%
  mutate(error_type = ifelse(!is.na(QTE1_1),"Control",
                             ifelse(!is.na(QTE2_1),"Control",
                                    ifelse(!is.na(QTE3_1),"High",
                                           ifelse(!is.na(QTE4_1),"High",NA)))),
         treat = ifelse(!is.na(QTE1_1),"Authentic",
                        ifelse(!is.na(QTE2_1),"Fake",
                               ifelse(!is.na(QTE3_1),"Authentic",
                                      ifelse(!is.na(QTE4_1),"Fake",NA)))),
         response = as.numeric(ifelse(!is.na(QTE1_1),QTE1_1,
                                      ifelse(!is.na(QTE2_1),QTE2_1,
                                             ifelse(!is.na(QTE3_1),QTE3_1,
                                                    ifelse(!is.na(QTE4_1),QTE4_1,NA)))))) %>%
  filter(!is.na(response)) %>%
  mutate(response = as.numeric(response),
         yob = as.numeric(Q67),
         gender = Q2,
         attention = grepl("Indian Electoral Commission",Q314)) %>%
  select(colnames(data_co), Q314)

#### Models ####

# Run basic lm models on outcomes

# Mturk results
mturk_neutral <- tidy(lm(response ~ treat, data_mturk[data_mturk$error_type == "Control",]))
mturk_high <- tidy(lm(response ~ treat, data_mturk[data_mturk$error_type == "High",]))

# CESS Online results
co_neutral <- tidy(lm(response ~ treat, data_co[data_co$error_type == "Control",]))
co_high <- tidy(lm(response ~ treat, data_co[data_co$error_type == "High",]))

# Mturk incentivised results
mturk_neutral2 <- tidy(lm(response ~ treat, data_mturk2[data_mturk2$error_type == "Control",]))
mturk_high2 <- tidy(lm(response ~ treat, data_mturk2[data_mturk2$error_type == "High",]))

# Extra check subsetting data based on attention
mturk_neutral2_attention <- tidy(lm(response ~ treat, data_mturk2[data_mturk2$error_type == "Control" &
                                                                data_mturk2$Q314 == "The Indian Electoral Commission",]))
mturk_high2_attention <- tidy(lm(response ~ treat, data_mturk2[data_mturk2$error_type == "High" &
                                                                 data_mturk2$Q314 == "The Indian Electoral Commission",]))

#### Tables ####

## Table 3

# Results table:
mturk_neutral$mode <- "MTurk"
mturk_neutral$error <- "Control"
mturk_neutral$incentive <- "No"

mturk_high$mode <- "MTurk"
mturk_high$error <- "High"
mturk_high$incentive <- "No"

co_neutral$mode <- "CESS Online"
co_neutral$error <- "Control"
co_neutral$incentive <- "No"

co_high$mode <- "CESS Online"
co_high$error <- "High"
co_high$incentive <- "No"

mturk_neutral2$mode <- "MTurk"
mturk_neutral2$error <- "Control"
mturk_neutral2$incentive <- "Yes"

mturk_high2$mode <- "MTurk"
mturk_high2$error <- "High"
mturk_high2$incentive <- "Yes"

results_table <- rbind(mturk_neutral, mturk_high, co_neutral, co_high, mturk_neutral2, mturk_high2) %>%
  filter(term != "(Intercept)") %>%
  rename(Coefficient = estimate,
         S.E. = std.error,
         `t-statistic` = statistic,
         p = p.value,
         Mode = mode,
         `Error Manipulation` = error,
         `Incentivised?` = incentive) %>%
  select(-term)

print(xtable(results_table, digits = 2,
             caption = "Induced measurement error model results", 
             label = "tab:india_results",
             align = "llllllll"),
      include.rownames = FALSE,
      hline.after = c(-1,0,nrow(results_table)-2,nrow(results_table)),
      table.placement = "",
      file = "tables/table_3.tex")

#### Appendix tables ####

## Table B4

mturk_desc <- data_mturk %>%
  group_by(treat, error_type) %>%
  summarise(N = n()) %>%
  spread(treat, N) %>%
  mutate(Mode = "MTurk",
         `Incentivised?` = "No")

mturk_desc2 <- data_mturk2 %>%
  group_by(treat, error_type) %>%
  summarise(N = n()) %>%
  spread(treat, N) %>%
  mutate(Mode = "MTurk",
         `Incentivised?` = "Yes")

co_desc <- data_co %>%
  group_by(treat, error_type) %>%
  summarise(N = n()) %>%
  spread(treat, N) %>%
  mutate(Mode = "CESS Online",
         `Incentivised?` = "No")

combined_desc <- rbind(mturk_desc, co_desc, mturk_desc2) %>%
  rename(`Authentic News (N)` = Authentic,
         `Fake News (N)` = Fake,
         `Error Manipulation` = error_type) %>%
  mutate(Total = `Authentic News (N)` + `Fake News (N)`) %>%
  select(Mode, `Error Manipulation`,`Incentivised?`,`Authentic News (N)`,`Fake News (N)`)

print(xtable(combined_desc,
             caption = "Number of participants in each mode and manipulation arm combination"),
      include.rownames = FALSE,
      hline.after = c(-1,0,nrow(combined_desc)-2,nrow(combined_desc)),
      file = "tables/table_b4.tex")


## TABLE B5

robust_mt_neutral <- lm(response ~ treat + yob + gender, data_mturk[data_mturk$error_type == "Control",])
robust_mt_high <-lm(response ~ treat + yob + gender, data_mturk[data_mturk$error_type == "High",])
robust_co_neutral <- lm(response ~ treat + yob + gender, data_co[data_co$error_type == "Control",])
robust_co_high <- lm(response ~ treat + yob + gender, data_co[data_co$error_type == "High",])
robust_mturk2_neutral <- lm(response ~ treat + yob + gender, data_mturk2[data_mturk2$error_type == "Control",])
robust_mturk2_high <- lm(response ~ treat + yob + gender, data_mturk2[data_mturk2$error_type == "High",])

stargazer(robust_mt_neutral, robust_mt_high,
          robust_co_neutral, robust_co_high,
          robust_mturk2_neutral, robust_mturk2_high,
          
          float.env = "sidewaystable",
          
          title = "Regression results for Indian vignette experiment with age and gender controls",
          label = "tab:india_robust",
          
          dep.var.labels = "Model",
          
          
          covariate.labels = c("Treat","Age", "Gender: Male", "Gender: Other", "Constant"),
          
          add.lines = list(c("Error Arm","Control","High","Control","High","Control","High"),
                           c("Mode","MTurk","MTurk","CESS Online","CESS Online","MTurk", "MTurk"),
                           c("Incentivised?","No","No","No","No","Yes","Yes")),
          
          star.cutoffs  =  c(NA,NA,NA),
          omit.table.layout = "n",
          
          omit.stat = c("rsq","ser","f"),
          
          digits = 2,
          
          out="tables/table_b5.tex")

## Table B6

mturk_neutral2_attention_model <- lm(response ~ treat, data_mturk2[data_mturk2$error_type == "Control" &
                                                                     data_mturk2$Q314 == "The Indian Electoral Commission",])
mturk_high2_attention_model <- lm(response ~ treat, data_mturk2[data_mturk2$error_type == "High" &
                                                                 data_mturk2$Q314 == "The Indian Electoral Commission",])

stargazer(mturk_neutral2_attention_model,
          mturk_high2_attention_model,
          
          label = "tab:india_robust_attention_iec_only",
          title = "Regression results for high attention subjects in attention-incentivised arm (where participant identified \\textit{only} the Indian Electoral Commission).",
          
          covariate.labels = c("Treat", "Constant"),
          
          dep.var.caption = "Model",
          dep.var.labels.include = FALSE,
          
          add.lines = list(c("Error Arm","Control","High"),
                           c("Mode","MTurk","MTurk"),
                           c("Incentivised?","Yes","Yes")),
          
          star.cutoffs  =  c(NA,NA,NA),
          omit.table.layout = "n",
          
          digits =  2,
          
          out="tables/table_b6.tex")

