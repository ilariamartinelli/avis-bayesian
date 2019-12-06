
#**************************************************************************************
#*******************   BAYES PROJECT - AVIS RECURRENT EVENTS **************************
#**************************************************************************************


rm(list=ls())

# Libraries

library(tidyverse)
library(ggplot2)

load("C:/Users/39334/Downloads/BAYESIAN/PROJECT/avis_clean_plus.RData")
attach(avis)




# TIME - INDEPENDENT COVARIATES ------------------------------------------------------------------------

#We consider an unique time-independent value for each donor, by creating a suitable dataset

unique_donors = avis %>% group_by(CAI) %>%  summarise(num=n(), sex=unique(SESSO), fumo=unique(FUMO),alcool=unique(ALCOOL),
        the=unique(THE),caffe=unique(CAFFE), dieta=unique(DIETA),stress=unique(STRESS),attivitafisica=unique(ATTIVITAFISICA),
        altezza=unique(ALTEZZA),peso=unique(PESO),
        tiposangue=unique(AB0), rh=unique(RH),etaprima=unique(eta_prima) ) 
attach(unique_donors)


 
#________________________________________________________________________________________________
# Recurrences
range(unique_donors[,2])
as.matrix(table(unique_donors$num)/length(unique_donors$num))
sum( as.vector(table(unique_donors$num)/length(unique_donors$num))[c(12:28)] ) # freq >13
unique_donors$sex=factor(unique_donors$sex,labels=c("men","women"))

p <- ggplot(unique_donors, aes(x=sex, y=num)) + 
  geom_boxplot(fill=c("dodgerblue","indianred1")) + 
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5))+
  ggtitle("Total Recurrences")
p


#________________________________________________________________________________________________
#Sex
length(which(is.na(sex)==1)) # no missing information
table(sex)


#___________________________________________________________________________________________________
# Fumo
CAI[which(is.na(fumo)==TRUE)]
length(which(is.na(fumo)==TRUE)) #dati mancanti
as.matrix(table(factor(fumo,levels=c("No","ex da 10 anni","ex da 3 anni","ex da 1 anno","- di 10 sig/die","< 5 sigarette/die",
                           "5-10 sigarette/die","10-20 sigarette/die","20-30 sigarette/die","> 30 sigarette/die","pipa o sigaro")))/length(which(is.na(fumo)==FALSE)))

fumo.label=factor(ifelse(fumo %in% c("No","ex da 10 anni","ex da 3 anni","ex da 1 anno"), 'No Smoker', 'Smoker'))
as.matrix(table(fumo.label)/length(fumo))

p <- ggplot(unique_donors, aes(x=fumo.label, y=num)) + 
  geom_boxplot(fill=c("ivory1","gray37")) + 
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5))+
  ggtitle("Total Recurrences")
p

rm(fumo.label)


#___________________________________________________________________________________________________
# Alcool
CAI[which(is.na(alcool)==TRUE)]    #-> noto che sono esatt. gli stessi donatori di prima!!
length(which(is.na(alcool)==TRUE)) #dati mancanti
levels(factor(alcool))
as.matrix(table(factor(alcool,levels=c("No","Assunzione saltuaria","< 25 g/die","25-50 g/die", "50-10 g/die",
                                     "50-100 g/die")))/length(which(is.na(alcool)==FALSE)))

alcool.label=factor(ifelse(alcool %in% c("No","Assunzione saltuaria"), 'No Alcool', 'Alcool'))
as.matrix(table(alcool.label)/length(alcool))

p <- ggplot(unique_donors, aes(x=alcool.label, y=num)) + 
  geom_boxplot(fill=c("brown2","ivory1")) + 
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5))+
  ggtitle("Total Recurrences")
p

rm(alcool.label)


#___________________________________________________________________________________________________
# The and Caffe

#We can immediately decide to discard the and coffee because there are too many missing values

idx=unique( c( which(is.na(the)==TRUE), which(the=="NULL"))) # ho dovuto rimuovere anche i valori null
CAI[idx]   
length(idx) # noto che ne mancano tanti
levels(factor(the[-idx]))
as.matrix(table(factor(the[-idx],levels=c("No", "Non abitualmente","1/die","Circa 1/die", "Circa 2/die")))/length(the[-idx]))

idx=unique( c( which(is.na(caffe)==TRUE), which(caffe=="NULL"))) # ho dovuto rimuovere anche i valori null
CAI[idx]   
length(idx) # noto che ne mancano tanti
levels(factor(caffe[-idx]))
as.matrix(table(factor(caffe[-idx],levels=c("No","1/die","2/die","3/die","4/die","5/die",">5/die")))/length(caffe[-idx]))


#____________________________________________________________________________________________________
# Dieta
CAI[which(is.na(dieta)==TRUE)]   
length(which(is.na(dieta)==TRUE)) #dati mancanti
as.matrix(table(factor(dieta))/length(which(is.na(dieta)==FALSE)))


#____________________________________________________________________________________________________
# Stress
CAI[which(is.na(stress)==TRUE)]    
length(which(is.na(stress)==TRUE)) #dati mancanti
levels(factor(stress))
as.matrix(table(factor(stress))/length(which(is.na(dieta)==FALSE)))


#__________________________________________________________________________________________________
# Attivita fisica
CAI[which(is.na(attivitafisica)==TRUE)]    
length(which(is.na(attivitafisica)==TRUE)) #dati mancanti
as.matrix(table(factor(attivitafisica))/length(which(is.na(attivitafisica)==FALSE)))

attivitafisica.label=factor(ifelse(attivitafisica %in% c("Vita sedentaria","Sedentaria","Scarsa"), 'Sedentary Life', 'Active Life'))
as.matrix(table(attivitafisica.label)/length(attivitafisica))

p <- ggplot(unique_donors, aes(x=attivitafisica.label, y=num)) + 
  geom_boxplot(fill=c("lavender","ivory1")) + 
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5))+
  ggtitle("Total Recurrences")
p


#___________________________________________________________________________________________________
# Tipo sangue
length(which(is.na(tiposangue)==TRUE)) # non ci sono dati mancanti 
as.matrix(table(factor(tiposangue))/length(tiposangue))
plot(factor(tiposangue))

g <- ggplot(unique_donors, aes(factor(tiposangue)))+ geom_bar()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5))+
 # geom_text(aes(label=count), vjust=1.6, color="white", size=3.5)+
  ggtitle("Blood Type")
g 

p <- ggplot(unique_donors, aes(x=tiposangue, y=num)) + 
  geom_boxplot(fill=c("royalblue1","royalblue1","royalblue1","royalblue1")) + 
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5))+
  ggtitle("Total Recurrences")
p

# people ho have AB blood type go more often than other donors, 
# maybe because they know that their blood type is more rare 

#__________________________________________________________________________________________________
# RH
length(which(is.na(rh)==TRUE)) # non ci sono dati mancanti 
as.matrix(table(factor(rh))/length(rh))


#_________________________________________________________________________________________________
# Eta Prima

length(which(is.na(etaprima)==TRUE)) # non ci sono dati mancanti 
as.matrix(table(factor(etaprima))/length(etaprima))
summary(etaprima)

ggplot(unique_donors, aes(x=etaprima, color=sex, fill=sex)) +
  geom_histogram( position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values=c( "dodgerblue","indianred1"))+
  scale_fill_manual(values=c(  "dodgerblue","indianred1"))+
  ggtitle("Age First Donation")


#______________________________________________________________________________________________
# BMI 
BMI=peso/(altezza)^2




#*********************************************************************************************
# TO SUM UP:

# FEATURE            MISSING VALUES                 QUANTITY

# Sex                0                              Men           4005 
#                                                   Women         1932 

# Fumo               37                             No Smoker   0.67492
#                                                   Smoker      0.32508   

# Alcool             37  (stessi di fumo)           Alcool     0.3018359
#                                                   No Alcool  0.6981641

# The                1832  (troppi)

# Caffe              1464  (troppi)

# Dieta              37

# Stress             37  (stessi di dieta)

# Att Fis            37  (stessi di dieta)        Active Life    0.7603167
#                                                 Sedentary Life 0.2396833

# Tipo Sangue        0                            0            0.46403908
#                                                 A            0.39666498
#                                                 AB           0.01364325
#                                                 B            0.12565269

# RH                 0                             NEG          0.1381169
#                                                  POS          0.8618831


#********************************************************************************************
# RECURRENCES (numero tot di volte che ha donato ciascun donatore)

# 2    0.2706754253
# 3    0.1852787603
# 4    0.1217786761
# 5    0.0934815563
# 6    0.0702374937
# 7    0.0490146539
# 8    0.0441300320
# 9    0.0299814721
# 10   0.0269496379
# 11   0.0200437932
# 12   0.0170119589
# 13   0.0126326427
# >13  0.07141654     (il massimo è 30)

#*********************************************************************************************



# TIME - DEPENDENT COVARIATES ------------------------------------------------------------------------

x11()
hist(as.numeric(num),breaks=30, xlim=c(1,30))

x11()
par(mfrow=c(1,4))
boxplot(PMIN)
boxplot(PMAX)
boxplot(POLSO)
boxplot(EMOG)

#fffffffff





