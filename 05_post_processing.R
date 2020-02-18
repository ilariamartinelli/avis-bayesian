#*******************************************************************
#*********************** POST PROCESSING ***************************
#*******************************************************************


rm(list=ls())

library(rstan)
library(coda)

# for plots
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(ggsci)
library(tidyverse)
library(lubridate)
library(bayesplot)
library(rstanarm)
require(gplots)
require(ggpubr)



# FIT ALL--------------------------------------------------------------------------------------------

load(file="fit_all.RData")
fit=fit.all
names(fit)
names(fit)[1:25]=c('lambda01','lambda02','lambda03','lambda04','lambda05','lambda06','lambda07','lambda08',
                   'lambda09','lambda10','Sesso','Fumo','Alcool','AttivitaFisica','Tipo0','TipoA',
                   'TipoB','TipoAB', 'RH','EtaPrima','BMI','Polso','Pmax','Emog','Pmin')
beta_names<-c('Sesso','Fumo','Alcool','AttivitaFisica','Tipo0','TipoA',
              'TipoB','TipoAB', 'RH','EtaPrima','BMI','Polso','Pmax','Emog','Pmin')

# 1 SESSO
# 2 FUMO
# 3 ALCOOL
# 4 ATTIVITA FISICA
# 5 TYPE 0
# 6 TYPE A
# 7 TYPE B
# 8 TYPE AB
# 9 RH
# 10 ETA PRIMA
# 11 BMI
# 12 POLSO
# 13 PMAX
# 14 EMOG
# 15 PMIN


# Beta---------------------------------------------------


beta_df= fit %>% rstan::extract("beta") %>% as.data.frame()
head(beta_df)
names(beta_df)=beta_names
plot_post_beta <- beta_df %>% map_df(as_data_frame, .id = 'param')


col=rep("grey",15)
col[c(4,6,8,10)]="blue"   # emog fumo and RH are negative
col[c(5,9,11)]='red'       # 

# IDEA: coloro di 3 colori diversi i beta positivi, negativi e nulli
x11()
plot_post_beta %>% 
  ggplot(aes(value, fill = param)) + 
  geom_density() + 
  facet_wrap(~param, scales = 'free') + 
  scale_fill_manual(values = col )+
  theme(legend.position="none")


print(fit, 
      probs = c(0.025, 0.5, 0.975), 
      par = c('beta'))

col=rep("grey",11)
col[c(2,5,10,11)]="blue"   
col[c(1,6,8)]='red'        

x11()
plot(fit, ask = T, pars = c("Sesso","Fumo","Alcool","AttivitaFisica","RH","EtaPrima","BMI",
                            "Polso","Pmax","Emog","Pmin"), ci_level = 0.95, fill_color = col)
# abline(v=0,col='red',lty=2)



# FIT --------------------------------------------------------------------------------------------

rm(fit.all)
rm(fit)

load(file="fit.RData")
names(fit)
names(fit)[1:17]=c('lambda01','lambda02','lambda03','lambda04','lambda05','lambda06','lambda07','lambda08',
                    'lambda09','lambda10','Sesso','Fumo','RH','EtaPrima','Polso','Emog','Pmin')
beta_names<-c('Sesso','Fumo', 'RH','EtaPrima','Polso','Emog','Pmin')

# 1 SESSO
# 2 FUMO
# 3 RH
# 4 ETA PRIMA
# 5 POLSO
# 6 EMOG
# 7 PMIN

#***************************************************************
# Beta---------------------------------------------------
#***************************************************************

beta_df= fit %>% rstan::extract("beta") %>% as.data.frame()
beta_df=beta_df[,-8]
head(beta_df)
names(beta_df)=beta_names
plot_post_beta <- beta_df %>% map_df(as_data_frame, .id = 'param')


col=rep("grey",7)
col[c(1,3,4,6)]="blue"   # emog fumo and RH are negative
col[c(2,5,7)]='red'       # 

# IDEA: coloro di 3 colori diversi i beta positivi, negativi e nulli
x11()
plot_post_beta %>% 
  ggplot(aes(value, fill = param)) + 
  geom_density() + 
  facet_wrap(~param, scales = 'free') + 
  scale_fill_manual(values = col)+
  theme(legend.position="none")

print(fit, 
      probs = c(0.025, 0.5, 0.975), 
      par = c('beta'))

col=rep("blue",7)
col[c(1,4,5)]='red'        

x11()
plot(fit, ask = T, pars = c("Sesso","Fumo","RH","EtaPrima",
                            "Polso","Emog","Pmin"), ci_level = 0.95, fill_color = col)
# abline(v=0,col='red',lty=2)



# TRACEPLOT
x11()
rstan::traceplot(fit, pars = c('Sesso','Fumo','RH','EtaPrima','Polso','Emog','Pmin'), inc_warmup = FALSE)

# ACF PLOT
x11()
par(mfrow=c(3,3))
for (i in 1:7) {
  acf(beta_df[,i],main=names(beta_df)[i],ylim=c(-0.1,0.1))
}



#***************************************************************
# Lambda----------------------------------------
#***************************************************************


lambda_df <- fit %>% rstan::extract("lambda") %>% as.data.frame() 
names(lambda_df)=c('lambda01','lambda02','lambda03','lambda04','lambda05','lambda06','lambda07','lambda08',
                   'lambda09','lambda10')
head(lambda_df)
plot_post_lambda=lambda_df %>% map_df(as_data_frame, .id = 'param')


x11()
plot_post_lambda %>% 
  ggplot(aes(value, fill = param)) + 
  geom_density() + 
  facet_wrap(~param, scales = 'free') + 
  #  scale_fill_locuszoom() +                   
  theme_bw() +
  scale_color_gradientn(colours = rainbow(5)) +
  theme(legend.position="none") 

print(fit, 
      probs = c(0.025, 0.5, 0.975), 
      par = c('lambda'))

x11()
plot(fit, ask = T, pars = "lambda", ci_level = 0.95, fill_color = "blue")


x11()
rstan::traceplot(fit, pars = "lambda", inc_warmup = FALSE)


x11()
par(mfrow=c(3,4))
for (i in 1:10) {
  acf(lambda_df[,i],main=names(lambda_df)[i],ylim=c(-0.1,0.1))
}



x = summary(fit, par='lambda')
lambda_mean = x[[1]][,'mean']
se_mean = x[[1]][,'se_mean']
n_eff = x[[1]][,9]

nodes = seq(1, 3101, length.out=11)

donations = c()
for(i in 1:10) {
  donations[i] = sum(nodes[i]<avis$time & avis$time<nodes[i+1])
}

# the baseline rate function follows the number of donations in each time interval   
# se vuoi per maggior chiarezza puoi mettere i plot in due figure distinte

x11()
par(mfrow=c(1,2))
plot(nodes[2:11], donations, type='s',xlab='time intervals',ylab='empirical donations')
plot(nodes[2:11], lambda_mean, type='s', xlab='time intervals', ylab='lambda posterior mean',col='red', lwd=2)




