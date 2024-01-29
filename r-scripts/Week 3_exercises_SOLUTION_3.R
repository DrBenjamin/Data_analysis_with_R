#### Diagnostic Testing methods - Exercise sample solution

# Activate packages needed for the analyses
library(reportROC)
library(tidyverse)


### 1 - Helicobacter example

helico<-read_csv(file = "Helicobacter.csv",col_names = T)
with(helico,table(Test,Biopsy))

reportROC(gold = helico$Biopsy,predictor.binary = helico$Test,plot=F,positive='l')

# New PPV and NPV obtained from formulae in lecture

sens<-0.93
spec<-0.832
prev<-0.6
ppv<-(sens*prev)/(sens*prev+(1-spec)*(1-prev))
npv<-(spec*(1-prev))/(spec*(1-prev)+(1-sens)*prev)
cbind(PPV = ppv,NPV = npv)


### 2 - AnginaMI example

angina<-read_csv(file = "AnginaMI.csv",col_names = T)

# Creatine Kinase is very skewed, so easier to see what's going on on the log scale
angina %>%
  ggplot(aes(y=log(CreatineKinase),x=Diagnosis))+
  geom_boxplot()

# Large values of Cr Ki associated with AMI, so positive="l". NB Analysis doesn't need to be on log scale!
reportROC(gold = angina$DiagnosisCat,predictor = angina$CreatineKinase,plot=T,positive='l')





