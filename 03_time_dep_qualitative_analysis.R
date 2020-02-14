#**********************************************************************************************************
#      TIME-DEPENDENT COVARIATES QUALITATIVE ANALYSIS
#**********************************************************************************************************

rm(list=ls())

# Libraries

library(tidyverse)
library(lubridate)
library(ggplot2)
library(coda)

# load:  "avis_clean_plus.RData"

attach(avis)

length(unique(avis$CAI)) # 5900 unique donors
donors=unique(avis$CAI)
# in order to select less donors, it's enough to use the command line:
# donors=unique(avis$CAI)[600:620] 
# instead of the line above (or any other randomly selected group of donors)
sex=sapply(avis %>% group_by(CAI) %>%  summarise(sex=unique(SESSO)),as.numeric)[,2]
recurrences=sapply(as.vector((avis %>% group_by(CAI) %>%  summarise(num=n()))[,2]),as.numeric)

df_continuous=(avis %>% group_by(CAI) )[c(23:26)]



col=rep('blue',length(donors))

for (i in 1:length(donors)){
  if (sex[i]==2)
    col[i]='red'
}

transparence=rep(0.1,length(donors))
  for (i in 1:length(donors)){
    if (sex[i]==2)
      transparence[i]=1
  }



# * Hemoglobin----------------------------------------------------------------


idx=1
v=rep(0,recurrences[1])
for (j in 1:recurrences[1]){
  v[j]=as.numeric(df_continuous[idx,1])
  idx=idx+1
}


x11()
par(mfrow=c(2,2))


plot(v,xlab='Recurrences',ylab='Hemoglobin',xlim=c(0,30),ylim=range(df_continuous[,1]),col=alpha(col[1],transparence[1]),type='l')

for (i in 2:length(donors)){
  v=rep(0,recurrences[i])
  for (j in 1:recurrences[i]){
    v[j]=as.numeric(df_continuous[idx,1])
    idx=idx+1
  }
  lines(v,xlim=c(0,30),ylim=range(df_continuous[,1]),col=alpha(col[i],transparence[i]))
}
abline(v=15,lty=2,col='black')
legend('topright',legend=c('Men','Women'),col=c("blue","red"),lty=c(1,1))


# * Pulse----------------------------------------------------------------


idx=1
v=rep(0,recurrences[1])
for (j in 1:recurrences[1]){
  v[j]=as.numeric(df_continuous[idx,2])
  idx=idx+1
}


plot(v,xlab='Recurrences',ylab='Pulse',xlim=c(0,30),ylim=range(df_continuous[,2]),col=alpha(col[1],transparence[1]),type='l')

for (i in 2:length(donors)){
  v=rep(0,recurrences[i])
  for (j in 1:recurrences[i]){
    v[j]=as.numeric(df_continuous[idx,2])
    idx=idx+1
  }
  lines(v,xlim=c(0,30),ylim=range(df_continuous[,2]),col=alpha(col[i],transparence[i]))
}
abline(v=15,lty=2,col='black')
legend('topright',legend=c('Men','Women'),col=c("blue","red"),lty=c(1,1))



# * Pmin----------------------------------------------------------------


idx=1
v=rep(0,recurrences[1])
for (j in 1:recurrences[1]){
  v[j]=as.numeric(df_continuous[idx,3])
  idx=idx+1
}


plot(v,xlab='Recurrences',ylab='Min Pressure',xlim=c(0,30),ylim=range(df_continuous[,3]),col=alpha(col[1],transparence[1]),type='l')

for (i in 2:length(donors)){
  v=rep(0,recurrences[i])
  for (j in 1:recurrences[i]){
    v[j]=as.numeric(df_continuous[idx,3])
    idx=idx+1
  }
  lines(v,xlim=c(0,30),ylim=range(df_continuous[,3]),col=alpha(col[i],transparence[i]))
}
abline(v=15,lty=2,col='black')
legend('topright',legend=c('Men','Women'),col=c("blue","red"),lty=c(1,1))



# * Pmax----------------------------------------------------------------


idx=1
v=rep(0,recurrences[1])
for (j in 1:recurrences[1]){
  v[j]=as.numeric(df_continuous[idx,4])
  idx=idx+1
}


plot(v,xlab='Recurrences',ylab='Max Pressure',xlim=c(0,30),ylim=range(df_continuous[,4]),col=alpha(col[1],transparence[1]),type='l')

for (i in 2:length(donors)){
  v=rep(0,recurrences[i])
  for (j in 1:recurrences[i]){
    v[j]=as.numeric(df_continuous[idx,4])
    idx=idx+1
  }
  lines(v,xlim=c(0,30),ylim=range(df_continuous[,4]),col=alpha(col[i],transparence[i]))
}
abline(v=15,lty=2,col='black')
legend('topright',legend=c('Men','Women'),col=c("blue","red"),lty=c(1,1))


# COMMENTS
# * Hemoglobin: clear distinction between men and women
# * All the others: no distinction between men and women
# In general: there isn't variability during time






















