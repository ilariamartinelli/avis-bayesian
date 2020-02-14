

#*******************************************************************
#***************** POST PROCESSING FIT 500  ************************
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
#require(gplots)
require(ggpubr)



load("fit-500.RData")
names(fit)[1:25]=c('lambda01','lambda02','lambda03','lambda04','lambda05','lambda06','lambda07','lambda08',
                                'lambda09','lambda10','Sesso','Fumo','Alcool','Attivita Fisica','Tipo 0','Tipo A',
                                'Tipo B','Tipo AB', 'RH','Eta Prima','BMI','Pmin','Pmax','Polso','Emog')
beta_names=c('Sesso','Fumo','Alcool','Attivita Fisica','Tipo 0','Tipo A',
             'Tipo B','Tipo AB', 'RH','Eta Prima','BMI','Pmin','Pmax','Polso','Emog')

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
# 12 PMIN
# 13 PMAX
# 14 POLSO
# 15 EMOG


#***************************************************************
# Beta---------------------------------------------------
#***************************************************************


# We have 100000 iteration, the first 50000 are warmup and with thin=25 -> 
# 4000 stored values, but the first 2000 are warmup, so weconsider only the last 2000

beta_df= fit %>% rstan::extract("beta") %>% as.data.frame()
names(beta_df)=beta_names
beta_df=beta_df[c(2001:4000),]
plot_post_beta <- beta_df %>% map_df(as_data_frame, .id = 'param')

x11()
par(mfrow=c(3,2))
for (i in c(1,10,5,6,7,8)) {
  acf(beta_df[,i],main=beta_names[i],ylim=c(-0.1,0.1))
}

x11()
plot_post_beta %>% 
  ggplot(aes(value, fill = param)) + 
  geom_density() + 
  facet_wrap(~param, scales = 'free') + 
  scale_fill_locuszoom() + 
  theme_minimal() +
  theme(legend.position="none")


# *********************************************************************************

# Plotto i beta significativi:
beta_df_significative=beta_df[,c(1,2,5,6,9,10)]
plot_post_beta_significative <- beta_df_significative %>% map_df(as_data_frame, .id = 'param')

x11()
plot_post_beta_significative %>% 
  ggplot(aes(value, fill = param)) + 
  geom_density() + 
  facet_wrap(~param, scales = 'free') + 
  scale_fill_locuszoom() + 
  theme_minimal() +
  theme(legend.position="none")

# TRACEPLOT

# Draw the traceplot corresponding to one or more Markov chains, 
# providing a visual way to inspect
# sampling behavior and assess mixing across chains and convergence.


# TRACEPLOTS 
x11()
color_scheme_set("red")
mcmc_trace(fit, pars = c('Sesso', 'Eta Prima',"Fumo","RH","Eta Prima","Tipo 0", "Tipo A" ),facet_args = list(ncol = 1, strip.position = "left"),inc_warmup = FALSE)




# *********************************************************************************
# Plotto i beta delle variabili tempo-dipendenti:
beta_df_td=beta_df[,c(11:15)]

x11()
print(fit, 
      probs = c(0.025, 0.5, 0.975), 
      par = c('beta'))
plot(fit, ask = T, pars = "beta", ci_level = 0.95, fill_color = "blue")





#***************************************************************
# Lambda----------------------------------------
#***************************************************************


lambda_df <- fit %>% rstan::extract("lambda") %>% as.data.frame() 
lambda_df=lambda_df[c(2001:4000),]
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


x11()
color_scheme_set("red")
mcmc_trace(fit, pars = c('lambda01'),facet_args = list(ncol = 1, strip.position = "left"),inc_warmup = FALSE)



x11()
print(fit, 
      probs = c(0.025, 0.5, 0.975), 
      par = c('lambda'))
plot(fit, ask = T, pars = "lambda", ci_level = 0.95, fill_color = "blue")



x11()
par(mfrow=c(3,))
for (i in 1:10) {
  acf(lambda_df[,i],main=names(lambda_df)[i],ylim=c(-0.1,0.1))
}


#**************************************************************************************
# Frailties----------------------------------------------------------------------------
#**************************************************************************************


w_df <- fit %>% rstan::extract("w") %>% as.data.frame() 
w_df=w_df[c(2001:4000),]
post_mean_w=log(sapply(w_df,mean))

# Load avis.RData
avis=avis_new
num_tot=(avis %>% group_by(CAI) %>%  summarise(num=n()))$num
num=log(num_tot[c(1:500)]) # I select the first 500 donors

df.recurrences=data.frame(cbind(num,post_mean_w))
ggplot(df.recurrences,aes(x=num,y=post_mean_w)) +
       geom_point() +
       labs(x='log(recurrences)',y='log(w_post_mean)') +
       geom_smooth(method='lm',se=FALSE)

# It seems that there is a linear association between the number of recurrences of 
# each donor and the frailty's posterior mean. Every individual in the dataset
# contributes to estimate the variance ´ of the random effects' population, and so it is
# possible to estimate the predictive density of a new incoming donor's frailty wnew (see
# The random effects are spread in a wide range
# So it is noticeable that there is heterogeneity 
# between individuals that is not captured by observable features






