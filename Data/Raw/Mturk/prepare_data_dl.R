
################################################ 
### Data Management for Lab students online sync
### March 2017
### Denise Laroze
################################################




library(plyr)
library(foreign)


#setwd("C:/Users/André Laroze/Dropbox/CESS-Santiago/Archive/Modes/Tax Cheating Qualtrics Online Experiment/Data/Mturk  Interactive Online Tax/data_processing")#################################
setwd("C:/Users/Denise Laroze Prehn/Dropbox/CESS-Santiago/Archive/Modes/Tax Cheating Qualtrics Online Experiment/Data/Mturk  Interactive Online Tax/data_processing")#################################



rm(list = ls())


##################################
### Generating and merging files
#################################


## RET and Report
round.files <- list.files("./data/", recursive = T, pattern = 'Module2_6.+csv', full.names = T)
 
all.round <- NULL
for(bf in round.files){
  temp <- read.csv(bf, as.is = T)
  for(i in 1:ncol(temp))temp[ , i] <- gsub("\\s","", temp[, i])
  rn <- sub(".+/(room_.+?)/.+", "\\1", bf)
  sn <- sub(".+/data\\.(session\\d)/.+", "\\1", bf)
  temp$room <- rn
  temp$session <- sn
  round_number <- as.numeric(sub(".+/.+\\.(\\d+).csv", "\\1", bf))
  temp$round_number <- round_number
  all.round <- rbind.fill(all.round, temp)
}

table(table(all.round$player))
room_obs <-table(all.round$room)
complete_rooms <- names(room_obs)[which(room_obs==40)]
all.round$complete_rooms_module2 <- all.round$room %in% complete_rooms
str(all.round)
names(all.round)
numerize <- names(all.round)[c(5,6,7,8,10,11,12)]
for(nu in numerize){
  all.round[,nu]<- as.numeric(all.round[ ,nu])
}
table(all.round$complete_rooms_module2)

all.round$cheat <- ifelse(all.round$preEarnings==all.round$declareEarnings, 0, 1)
table(all.round$cheat)
all.round$report.rate <- all.round$declareEarnings/all.round$preEarnings

table(all.round$report.rate==1)
table(all.round$report.rate==0)

all.round$muID<-paste0(all.round$player, all.round$session, all.round$room)



#### Participant characteristics
round.files <- list.files("./data/", recursive = T, pattern = 'memory_dataResult_8.+csv', full.names = T)

all.round.2 <- NULL
for(bf in round.files){
  temp <- read.csv(bf, as.is = T)
  for(i in 1:ncol(temp))temp[ , i] <- gsub("\\s","", temp[, i])
  rn <- sub(".+/(room_.+?)/.+", "\\1", bf)
  sn <- sub(".+/data\\.(session\\d)/.+", "\\1", bf)
  temp$room <- rn
  temp$session <- sn
  round_number <- as.numeric(sub(".+/.+\\.(\\d+).csv", "\\1", bf))
  temp$round_number <- round_number
  all.round.2 <- rbind.fill(all.round.2, temp)
}

all.round.2$muID<-paste0(all.round.2$player, all.round.2$session, all.round.2$room)


#### Dictator Game
round.files <- list.files("./data/", recursive = T, pattern = 'memory_Module1_3.+csv', full.names = T)

all.round.3 <- NULL
for(bf in round.files){
  temp <- read.csv(bf, as.is = T)
  for(i in 1:ncol(temp))temp[ , i] <- gsub("\\s","", temp[, i])
  rn <- sub(".+/(room_.+?)/.+", "\\1", bf)
  sn <- sub(".+/data\\.(session\\d)/.+", "\\1", bf)
  temp$room <- rn
  temp$session <- sn
  round_number <- as.numeric(sub(".+/.+\\.(\\d+).csv", "\\1", bf))
  temp$round_number <- round_number
  all.round.3 <- rbind.fill(all.round.3, temp)
  }

all.round.3$muID<-paste0(all.round.3$player, all.round.3$session, all.round.3$room)
all.round.3<- subset(all.round.3, input_value>=0 & timeup=="false")
all.round.3<-all.round.3[!duplicated(all.round.3$muID), ]

#### Risk preferences
round.files <- list.files("./data/", recursive = T, pattern = 'memory_Module4_7.+csv', full.names = T)

all.round.4 <- NULL
for(bf in round.files){
  temp <- read.csv(bf, as.is = T)
  for(i in 1:ncol(temp))temp[ , i] <- gsub("\\s","", temp[, i])
  rn <- sub(".+/(room_.+?)/.+", "\\1", bf)
  sn <- sub(".+/data\\.(session\\d)/.+", "\\1", bf)
  temp$room <- rn
  temp$session <- sn
  round_number <- as.numeric(sub(".+/.+\\.(\\d+).csv", "\\1", bf))
  temp$round_number <- round_number
  all.round.4 <- rbind.fill(all.round.4, temp)
}

all.round.4$muID<-paste0(all.round.4$player, all.round.4$session, all.round.4$room)
all.round.4<-all.round.4[!duplicated(all.round.4$muID), ]

r4<- data.frame( risk1 =  substr(all.round.4$arrayAnswers, start = 1, stop = 1),
                 risk2 =    substr(all.round.4$arrayAnswers, start = 4, stop = 4), 
                 risk3 =    substr(all.round.4$arrayAnswers, start = 7, stop = 7),
                 risk4 =    substr(all.round.4$arrayAnswers, start = 10, stop = 10),
                 risk5 =    substr(all.round.4$arrayAnswers, start = 13, stop = 13),
                 risk6 =    substr(all.round.4$arrayAnswers, start = 16, stop = 16),
                 risk7 =    substr(all.round.4$arrayAnswers, start = 19, stop = 19),
                 risk8 =    substr(all.round.4$arrayAnswers, start = 22, stop = 22),
                 risk9 =    substr(all.round.4$arrayAnswers, start = 25, stop = 25),
                 risk10 =    substr(all.round.4$arrayAnswers, start = 28, stop = 28)
                 )
r4$muID<-paste0(all.round.4$player, all.round.4$session, all.round.4$room)
r4<-subset(r4, !is.na(risk1))


#### Integrity score

round.files <- list.files("./data/", recursive = T, pattern = 'memory_questionary2_8.+csv', full.names = T)

all.round.5 <- NULL
for(bf in round.files){
  temp <- read.csv(bf, as.is = T)
  for(i in 1:ncol(temp))temp[ , i] <- gsub("\\s","", temp[, i])
  rn <- sub(".+/(room_.+?)/.+", "\\1", bf)
  sn <- sub(".+/data\\.(session\\d)/.+", "\\1", bf)
  temp$room <- rn
  temp$session <- sn
  round_number <- as.numeric(sub(".+/.+\\.(\\d+).csv", "\\1", bf))
  temp$round_number <- round_number
  all.round.5 <- rbind.fill(all.round.5, temp)
}

i5<- data.frame( integrity1 =  substr(all.round.5$arrayAnsers, start = 1, stop = 1),
                 integrity2 =    substr(all.round.5$arrayAnsers, start = 4, stop = 4), 
                 integrity3 =    substr(all.round.5$arrayAnsers, start = 7, stop = 7),
                 integrity4 =    substr(all.round.5$arrayAnsers, start = 10, stop = 10),
                 integrity5 =    substr(all.round.5$arrayAnsers, start = 13, stop = 13),
                 integrity6 =    substr(all.round.5$arrayAnsers, start = 16, stop = 16),
                 integrity7 =    substr(all.round.5$arrayAnsers, start = 19, stop = 19),
                 integrity8 =    substr(all.round.5$arrayAnsers, start = 22, stop = 22),
                 integrity9 =    substr(all.round.5$arrayAnsers, start = 25, stop = 25),
                 integrity10 =    substr(all.round.5$arrayAnsers, start = 28, stop = 28)
)

i5$muID<-paste0(all.round.5$player, all.round.5$session, all.round.5$room)



#### Die Results
round.files <- list.files("./data/", recursive = T, pattern = 'memory_dice_8.+csv', full.names = T)

all.round.6 <- NULL
for(bf in round.files){
  temp <- read.csv(bf, as.is = T)
  for(i in 1:ncol(temp))temp[ , i] <- gsub("\\s","", temp[, i])
  rn <- sub(".+/(room_.+?)/.+", "\\1", bf)
  sn <- sub(".+/data\\.(session\\d)/.+", "\\1", bf)
  temp$room <- rn
  temp$session <- sn
  round_number <- as.numeric(sub(".+/.+\\.(\\d+).csv", "\\1", bf))
  temp$round_number <- round_number
  all.round.6 <- rbind.fill(all.round.6, temp)
}

all.round.6$muID<-paste0(all.round.6$player, all.round.6$session, all.round.6$room)




#### Merging files
vars<-c("age","gender", "politics", "trust", "muID")
df<-merge(all.round, all.round.2[, vars], by="muID", all= T)

vars<-c("input_value",  "role", "other" , "muID")
df<-merge(df, all.round.3[, vars], by="muID" , all= T)

vars<-c("diceNumber" , "numberEntered" , "muID")
df<-merge(df, all.round.6[, vars], by="muID" , all= T)


df<-merge(df, r4, by="muID",  all = T)

df<-merge(df, i5, by="muID", all= T)

#Payment info
#pay.tmp<-subset(df, !session %in% c("session1", "session2"))
#write.csv(df, "C:/Users/André Laroze/Dropbox/CESS-Santiago/Archive/Modes/Tax Cheating Qualtrics Online Experiment/data/pay_list_2017.csv")


#################################################################################
#### renaming and adjusting relevant variables to df compatible with other modes
#################################################################################


names(df)[names(df)=="input_value"] <- "DictGive"
df$DictGive[df$input_value==-1]<-NA
df$auditRate<-ifelse(df$session %in% c("session1",  "session2"), 0, 0.1) # sessions 1 and 2 Audit rate 0,3-4 audit rate 0.1)
df$audited<-df$statusDeclare ## To make it compatible with other modes
df$treat<-ifelse(df$session %in% c("session1",  "session2"), 2, 1) # sessions 1 and 2 Audit rate 0,3-4 audit rate 0.1)


df$taxRate<-df$taxRate*100 ## To make it compatible with other modes
df$taxBracket[df$taxRate==10]<-3
df$taxBracket[df$taxRate==30]<-1

names(df)[names(df)=="preEarnings"] <- "prelimGain"


#class(df$report.rate)<-"numeric"

df$report.rate.mturk.ds<-df$report.rate
df$sample<-"Mturk-DS"
df$gender[df$gender==FALSE]<-NA
df$gender[df$gender=="-"]<-NA

#### Risk preferences
col_idx <- grep("^risk", names(df))
m1 <- as.matrix(df[,col_idx])
m1[m1== "A"] <- 0
m1[m1== "B"] <- 1
m1[m1== "-"] <- NA
class(m1)<-"numeric"
df[col_idx]  <- m1

df$risk.pref<-rowSums(m1, na.rm = T)

#### Integrity
col_idx <- grep("^integrity", names(df))
m1 <- as.matrix(df[,col_idx])
class(m1)<-"numeric"
df[col_idx]  <- m1

df$total.integrity<-rowSums(m1, na.rm = T)

#### Eliminating duplicated player-rounds ### These should not exist, but they do. 
tmp <- ( df[ , c("muID", "round")] )

df<-df[!duplicated(tmp), ]



write.csv(df, "C:/Users/Denise Laroze Prehn/Dropbox/CESS-Santiago/Archive/Modes/Tax Cheating Qualtrics Online Experiment/Data/Mturk_DS_Sept2017.csv")


