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



load("fit_all.RData")
fit=fit.all
names(fit)
names(fit)[1:25]=c('lambda01','lambda02','lambda03','lambda04','lambda05','lambda06','lambda07','lambda08',
                   'lambda09','lambda10','Sesso','Fumo','Alcool','Attivita Fisica','Tipo 0','Tipo A',
                   'Tipo B','Tipo AB', 'RH','Eta Prima','BMI','Polso','Pmax','Emog','Pmin')
beta_names<-c('Sesso','Fumo','Alcool','Attivita Fisica','Tipo 0','Tipo A',
              'Tipo B','Tipo AB', 'RH','Eta Prima','BMI','Polso','Pmax','Emog','Pmin')

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


#***************************************************************
# Beta---------------------------------------------------
#***************************************************************


# We have 100000 iteration, the first 50000 are warmup and with thin=25 -> 
# 4000 stored values, but the first 2000 are warmup, so weconsider only the last 2000

beta_df= fit %>% rstan::extract("beta") %>% as.data.frame()
head(beta_df)
names(beta_df)=beta_names
plot_post_beta <- beta_df %>% map_df(as_data_frame, .id = 'param')





# IDEA: coloro di 3 colori diversi i beta positivi, negativi e nulli
x11()
plot_post_beta %>% 
  ggplot(aes(value, fill = param)) + 
  geom_density() + 
  facet_wrap(~param, scales = 'free') + 
  #  scale_fill_locuszoom() +                   
  theme_bw() +
  scale_color_gradientn(colours = rainbow(5)) +
  theme(legend.position="none") 


print(fit, 
      probs = c(0.025, 0.5, 0.975), 
      par = c('beta'))
x11()
plot(fit, ask = T, pars = "beta", ci_level = 0.95, fill_color = "blue")
plot(fit, ask = T, pars = c('beta[1]','beta[2]'), ci_level = 0.95, fill_color = "blue")



# *********************************************************************************
#  Beta significative: 

# POSITIVE EFFECT:
#  - Age first  (same results in the thesis)
#  - Sex        (same results in the thesis)

# NEGATIVE EFFECT:
#  - Smoke (same resuls in the thesis)
#  - Emog


# NULL EFFECT:
#  - Alcool
#  - Attivita Fisica
#  - BMI
#  - Pmax
#  - Pmin
#  - RH: negative effect in the thesis, for us is sligthly negative, but zero is included)
#  - Pulse: slightly positive effect, but zero is included
#  - Blood type (very strong evidence)

# Plotto i beta significativi:
names(beta_df)
beta_df_significative=beta_df[,-c(3,4,9,11,12,13,15)] 
names(beta_df_significative)
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


# TRACEPLOTS FOR AGE AND SEX
x11()
color_scheme_set("blue")
mcmc_trace(fit, pars = c('Sesso', 'Eta Prima'),facet_args = list(ncol = 1, strip.position = "left"),inc_warmup = FALSE)


# TRACEPLOTS FOR TYPE 0, A, B, AB
x11()
color_scheme_set("red")
mcmc_trace(fit, pars = c('Sesso',"Eta Prima","Tipo 0", "Tipo A","Tipo B","Tipo AB" ),facet_args = list(ncol = 1, strip.position = "left"),inc_warmup = FALSE)




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
  acf(lambda_df[,i],main=names(lambda_df)[i])
}

# Frailties----------------------------------------------------------------------------

w_df <- fit %>% rstan::extract("w") %>% as.data.frame() 
post_mean_w=log(sapply(w_df,mean))

# Carico avis_data.RData
avis=avis.df
num_tot=(avis %>% group_by(CAI) %>%  summarise(num=n()))$num
num=log(num_tot[c(1:500)]) # I select the first 500 donors

df.recurrences=data.frame(cbind(num,post_mean_w))
x11()
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



##########     DIAGNOSTICS    #########
library(coda)
print(fit.all, 
      probs = c(0.025, 0.5, 0.975), 
      par = c('beta', 'lambda'))

rstan::traceplot(fit.all, pars = "beta", inc_warmup = FALSE)
rstan::traceplot(fit.all, pars= "lambda", inc_warmup = FALSE)

coda_chain <- As.mcmc.list(fit.all, pars = c("beta", "lambda"))
summary(coda_chain)

gelman.diag(coda_chain, confidence = 0.95,  autoburnin = TRUE, multivariate=TRUE)
acfplot(coda_chain, lag.max = 30)
