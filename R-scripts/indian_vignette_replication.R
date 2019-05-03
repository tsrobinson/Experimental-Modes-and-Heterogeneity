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
library(tidyverse)
library(broom)
library(xtable)
library(stargazer)

#### Data formatting ####

# MTurk data formatting

data_mturk <- read_csv("data/mturk_exp.csv") %>%
  .[-c(1:2),] %>%
  filter(grepl("01/04/2019",.$StartDate)) %>%
  mutate(error_type = ifelse(!is.na(QTE1_1),"Neutral",
                                ifelse(!is.na(QTE2_1),"Neutral",
                                       ifelse(!is.na(QTE3_1),"High",
                                              ifelse(!is.na(QTE4_1),"High",NA)))),
         treat = ifelse(!is.na(QTE1_1),"Control",
                            ifelse(!is.na(QTE2_1),"Treat",
                                   ifelse(!is.na(QTE3_1),"Control",
                                          ifelse(!is.na(QTE4_1),"Treat",NA)))),
         response = as.numeric(ifelse(!is.na(QTE1_1),QTE1_1,
                                      ifelse(!is.na(QTE2_1),QTE2_1,
                                             ifelse(!is.na(QTE3_1),QTE3_1,
                                                    ifelse(!is.na(QTE4_1),QTE4_1,NA)))))) %>%
  filter(!is.na(response)) %>%
  mutate(response = as.numeric(response),
         yob = as.numeric(Q67),
         gender = Q2)

# CESS Online formatting

data_co <- read_csv("data/co_exp.csv") %>%
  rename(QTE1_1 = Q510_1,
         QTE2_1 = Q511_1,
         QTE3_1 = Q512_1,
         QTE4_1 = Q513_1) %>%
  select(QTE1_1,QTE2_1,QTE3_1,QTE4_1,
         yob, gender) %>%
  mutate(error_type = ifelse(!is.na(QTE1_1),"Neutral",
                             ifelse(!is.na(QTE2_1),"Neutral",
                                    ifelse(!is.na(QTE3_1),"High",
                                           ifelse(!is.na(QTE4_1),"High",NA)))),
         treat = ifelse(!is.na(QTE1_1),"Control",
                        ifelse(!is.na(QTE2_1),"Treat",
                               ifelse(!is.na(QTE3_1),"Control",
                                      ifelse(!is.na(QTE4_1),"Treat",NA)))),
         response = as.numeric(ifelse(!is.na(QTE1_1),QTE1_1,
                                      ifelse(!is.na(QTE2_1),QTE2_1,
                                             ifelse(!is.na(QTE3_1),QTE3_1,
                                                    ifelse(!is.na(QTE4_1),QTE4_1,NA)))))) %>%
  filter(!is.na(response)) %>%
  mutate(response = as.numeric(response))

# Incentivised MTurk formatting

data_mturk2 <- read_csv("data/mturk_exp_incentivised.csv") %>%
  .[-c(1:2),] %>%
  filter(Finished == TRUE) %>%
  mutate(error_type = ifelse(!is.na(QTE1_1),"Neutral",
                             ifelse(!is.na(QTE2_1),"Neutral",
                                    ifelse(!is.na(QTE3_1),"High",
                                           ifelse(!is.na(QTE4_1),"High",NA)))),
         treat = ifelse(!is.na(QTE1_1),"Control",
                        ifelse(!is.na(QTE2_1),"Treat",
                               ifelse(!is.na(QTE3_1),"Control",
                                      ifelse(!is.na(QTE4_1),"Treat",NA)))),
         response = as.numeric(ifelse(!is.na(QTE1_1),QTE1_1,
                                      ifelse(!is.na(QTE2_1),QTE2_1,
                                             ifelse(!is.na(QTE3_1),QTE3_1,
                                                    ifelse(!is.na(QTE4_1),QTE4_1,NA)))))) %>%
  filter(!is.na(response)) %>%
  mutate(response = as.numeric(response),
         yob = as.numeric(Q67),
         gender = Q2,
         attention = grepl("Indian Electoral Commission",Q314))


#### Models ####

# Mturk results

mturk_neutral <- tidy(lm(response ~ treat, data_mturk[data_mturk$error_type == "Neutral",]))
mturk_high <- tidy(lm(response ~ treat, data_mturk[data_mturk$error_type == "High",]))

# CESS Online results

co_neutral <- tidy(lm(response ~ treat, data_co[data_co$error_type == "Neutral",]))
co_high <- tidy(lm(response ~ treat, data_co[data_co$error_type == "High",]))

# Mturk incentivised results

mturk_neutral2 <- tidy(lm(response ~ treat, data_mturk2[data_mturk2$error_type == "Neutral",]))
mturk_high2 <- tidy(lm(response ~ treat, data_mturk2[data_mturk2$error_type == "High",]))

## Extra check subsetting data based on attention 
mturk_neutral2_attention <- tidy(lm(response ~ treat, data_mturk2[data_mturk2$error_type == "Neutral" &
                                                                data_mturk2$Q314 == "The Indian Electoral Commission",]))
mturk_high2_attention <- tidy(lm(response ~ treat, data_mturk2[data_mturk2$error_type == "High" &
                                                                 data_mturk2$Q314 == "The Indian Electoral Commission",]))

#### Tables in paper ####

## Table 6

# Results table:
mturk_neutral$mode <- "MTurk"
mturk_neutral$error <- "Neutral"
mturk_neutral$incentive <- "No"

mturk_high$mode <- "MTurk"
mturk_high$error <- "High"
mturk_high$incentive <- "No"

co_neutral$mode <- "CESS Online"
co_neutral$error <- "Neutral"
co_neutral$incentive <- "No"

co_high$mode <- "CESS Online"
co_high$error <- "High"
co_high$incentive <- "No"

mturk_neutral2$mode <- "MTurk"
mturk_neutral2$error <- "Neutral"
mturk_neutral2$incentive <- "Yes"

mturk_high2$mode <- "MTurk"
mturk_high2$error <- "High"
mturk_high2$incentive <- "Yes"

results_table <- rbind(mturk_neutral, mturk_high, co_neutral, co_high, mturk_neutral2, mturk_high2) %>%
  filter(term != "(Intercept)") %>%
  rename(Coefficient = estimate,
         SE = std.error,
         t = statistic,
         p = p.value) %>%
  select(-term)

print(xtable(results_table, digits = 2),
      include.rownames = FALSE)

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
  rename(Treatment = Treat,
         Error = error_type) %>%
  select(Mode, Error, `Incentivised?`,Control, Treatment) %>%
  mutate(Total = Control + Treatment)

print(xtable(combined_desc),
      include.rownames = FALSE)


## TABLE B5

robust_mt_neutral <- lm(response ~ treat + yob + gender, data_mturk[data_mturk$error_type == "Neutral",])
robust_mt_high <-lm(response ~ treat + yob + gender, data_mturk[data_mturk$error_type == "High",])
robust_co_neutral <- lm(response ~ treat + yob + gender, data_co[data_co$error_type == "Neutral",])
robust_co_high <- lm(response ~ treat + yob + gender, data_co[data_co$error_type == "High",])
robust_mturk2_neutral <- lm(response ~ treat + yob + gender, data_mturk2[data_mturk2$error_type == "Neutral",])
robust_mturk2_high <- lm(response ~ treat + yob + gender, data_mturk2[data_mturk2$error_type == "High",])

stargazer(robust_mt_neutral, robust_mt_high,
          robust_co_neutral, robust_co_high,
          robust_mturk2_neutral, robust_mturk2_high,
          
          label = "tab:india_robust",
          
          add.lines = list(c("Error","Neutral","High","Neutral","High","Neutral","High"),
                           c("Mode","MTurk","MTurk","CESS Online","CESS Online","MTurk", "MTurk"),
                           c("Incentivised?","No","No","No","No","Yes","Yes")))
