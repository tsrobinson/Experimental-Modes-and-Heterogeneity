
################################################ 
### Data Management for Lab students online sync
### March 2017
### Denise Laroze
################################################




setwd("C:/Users/Denise Laroze Prehn/Dropbox/CESS-Santiago/archive/Modes/Tax Cheating Qualtrics Online Experiment/Data/online interactive data - lab subjects")


library(plyr)
library(foreign)

rm(list = ls())

#################################
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
r4$player<-all.round.4$player

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

i5$player<-all.round.5$player



#### Merging files
vars<-c("age","gender", "politics", "trust", "player")
df<-merge(all.round, all.round.2[, vars], by="player", all= T)

vars<-c("input_value",  "role", "other" , "player")
df<-merge(df, all.round.3[, vars], by="player" , all= T)


df<-merge(df, r4, by="player",  all = T)
df<-merge(df, i5, by="player", all= T)



#################################################################################
#### renaming and adjusting relevant variables to df compatible with other modes
#################################################################################


names(df)[names(df)=="input_value"] <- "DictGive"
df$DictGive[df$input_value==-1]<-NA
#df$auditRate<-10## To make it compatible with other modes
df$auditRate<-ifelse(df$room %in% c("room_1489610940008", "room_1489611177729", "room_1489611569280",  "room_1489611702259" ,   
                                    "room_1489612403645",  "room_1489612930406", "room_1489613233524", "room_1489613861443", 
                                    "room_1489615291420", "room_1489616858980",  "room_1489688974211",  "room_1489689554622"   
                                    ), 0.1, 0) # In these rooms audit rate 0.1; others audit rate 0
df$audited<-df$statusDeclare ## To make it compatible with other modes
df$taxRate<-df$taxRate*100 ## To make it compatible with other modes

df$taxBracket[df$taxRate==10]<-3
df$taxBracket[df$taxRate==30]<-1

names(df)[names(df)=="preEarnings"] <- "prelimGain"
names(df)[names(df)=="player"] <- "muID"


#class(df$report.rate)<-"numeric"

df$report.rate.lab.online.sync<-df$report.rate
df$sample<-"Online Sync"
df$treat<-ifelse(df$auditRate >0, 1, 2)
df$gender[df$gender==F]<-NA

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

write.csv(df, "C:/Users/Denise Laroze Prehn/Dropbox/CESS-Santiago/Archive/Modes/Tax Cheating Qualtrics Online Experiment/Data/lab_online_sync_edited.csv")


