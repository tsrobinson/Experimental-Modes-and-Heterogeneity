

library(foreign)


setwd("C:/Users/André Laroze/Dropbox/CESS-Santiago/Archive/Tax Cheating Qualtrics Online Experiment/Data")
 

online.lab.s<-read.csv("online_labstudents_dl.csv", sep = ";")

# variables to reshape
myvar<-dput(names(online.lab.s))
vars<-c(paste0("audited.", 1:10, sep=""), paste0("groupSum.", 1:10, sep=""),
       paste0("taxBracket.", 1:10, sep=""), paste0("prelimGain.", 1:10, sep=""),
       paste0("decGain.", 1:10, sep="")
)



### Eliminating duplicated ids - First eliminated people with no declaired gains (relevant info), then kept only the first observation
online.lab.s2<-online.lab.s[complete.cases(online.lab.s$decGain.1), ]
online.lab.s2<- online.lab.s2[order(online.lab.s2$muID),] 
online.lab.s2<-online.lab.s2[!duplicated(online.lab.s2$muID), ]

idvar<-myvar[!myvar %in% vars]

ols.w<-reshape(online.lab.s2, idvar=c("muID"), varying = vars, 
              direction="long", sep = ".", timevar="round")


#ordering and creating relevant variables
ols.w<- ols.w[order(ols.w$muID, ols.w$round),] 
ols.w$sub_round<-rep(1:5)

####Audit treatment 1 audit rate >0, 2 audit rate =0
ols.w$treat<-1
ols.w$treat[ols.w$round %in% 6:10]<-2

#### Risk preferences
col_idx <- grep("^Risk_", names(ols.w))
m1 <- as.matrix(ols.w[,col_idx])
m1[m1== 1] <- 0
m1[m1== 2] <- 1
ols.w[col_idx]  <- m1


m1<-as.data.frame(m1)
ols.w$risk.pref<-rowSums(m1, na.rm = T)

#### Integrity

ols.w$total.integrity<-rowSums(ols.w[,grep("^Integrity_", names(ols.w))])

#### TaxRate
ols.w$TaxRate<-NA

for (i in 1:nrow(ols.w)){
  ols.w$TaxRate[i]<-ifelse(ols.w$taxBracket[i]==1, ols.w$taxRate1[i], 
                          ifelse(ols.w$taxBracket[i]==2, ols.w$taxRate2[i], ols.w$taxRate3[i] ))
  
}



#### Report Rate

ols.w$online.student.report.rate<-ols.w$decGain/ols.w$prelimGain
ols.w$online.student.report.rate[ols.w$online.student.report.rate>1]<-1

write.csv(ols.w, "online_student.csv")


