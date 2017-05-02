#Chapter7 e1.
library(readr)
BURN13M <- read_delim("C:/Users/juho/Desktop/ALR/Hosmer&Lemeshow_Data/BURN/BURN13M.txt", 
  "\t", escape_double = FALSE, trim_ws = TRUE)
#This is a case-control dataset where each stratum contains 3 controls for 1 case. A case being a death from burns.
#The assignment is to use only one control per case, and do a conditional logistic regression eliminating the nuisance alphas.
n<-seq(1,385,4) #the rows containing the controls
palo<-data.frame();for(i in n){palo0<-rbind(BURN13M[i,],BURN13M[i+3,]);palo<-rbind(palo,palo0)} #Binding together one case below each control.
palo$FACILITY<-as.factor(palo$FACILITY) #No sense having this as numeric. It's an ID for the hospital.
summary(matchedpairs<-clogit(DEATH ~ TBSA + INH_INJ + FLAME + RACEC + GENDER + strata(PAIR), data=palo))
