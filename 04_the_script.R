library(tidyverse)
library(lubridate)

load(file='donazioni_full.RData')
load(file='time_dependent.RData')

time_dependent$DTPRES = as.character(time_dependent$DTPRES)
time_dependent$DTPRES = dmy_hms(time_dependent$DTPRES)

# why is it adding observations to donazioni_full if we are using a left join?
avis = left_join(donazioni_full, time_dependent)

# if we look at the variable DTPRES we can see that there are repeated donations (same DTPRES value).
# BUG?
wrong_CAI = c(357695, 436922, 469875)
wrong = avis[avis$CAI %in% wrong_CAI,]

# observe that now avis and donazioni_full have the same number of rows
avis = avis %>% slice(-c(5649:5663, 24180:24194, 30835:30849))

# NA-values present, max values are absurd/wrong
summary(avis[c('PMIN', 'PMAX', 'POLSO', 'EMOG')])

#######    remove donors that have absurd values     ########
wrong_pmin = avis %>% filter(PMIN > 200)
avis = avis %>% filter(!(CAI %in% wrong_pmin$CAI))

wrong_pmax = avis %>% filter(PMAX > 200)
avis = avis %>% filter(!(CAI %in% wrong_pmax$CAI))

wrong_polso = avis %>% filter(POLSO > 200)
avis = avis %>% filter(!(CAI %in% wrong_polso$CAI))

wrong_emog = avis %>% filter(EMOG > 25)
avis = avis %>% filter(!(CAI %in% wrong_emog$CAI))

#########     remove donors with only one donation     ##########
num_donaz = avis %>% group_by(CAI) %>% summarise(donazioni=n(), censoring=first(censoring))
num_donaz = num_donaz %>% filter(donazioni > 1)
avis = avis %>% filter(CAI %in% num_donaz$CAI)

#########   WARNING    ##########
# we make observations conditioned on the first donation
avis = avis %>% filter(time != 0) #?

# compute BMI. Weight in kg, height in meters
avis = avis %>% mutate(BMI = PESO/(ALTEZZA/100)^2)
avis = avis %>% filter(is.na(BMI) == FALSE)

########   remove NA-values   ########
# substitute NA with mean by donor and remove donors with all NA in one covariate

avis = avis %>% group_by(CAI) %>%
  mutate(pmin = ifelse(is.na(PMIN), mean(PMIN, na.rm=TRUE), PMIN))
summary(avis['pmin'])

avis = avis %>% filter(is.na(pmin) == FALSE)
summary(avis['pmin'])
avis$PMIN = NULL
avis = avis %>% rename('PMIN' = 'pmin')


# BUG? does not work
# avis = avis %>%
#        mutate(PMIN = ifelse(is.na(pmin), mean(pmin, na.rm=TRUE), pmin))
# summary(avis['PMIN'])

# mean_pmin = mean(avis$pmin, na.rm=TRUE)
# avis = avis %>%
#   mutate(PMIN = ifelse(is.na(pmin), mean_pmin, pmin))
# summary(avis['PMIN'])
# avis$pmin = NULL

avis = avis %>% group_by(CAI) %>%
  mutate(pmax = ifelse(is.na(PMAX), mean(PMAX, na.rm=TRUE), PMAX))
summary(avis['pmax'])

avis = avis %>% filter(is.na(pmax) == FALSE)
summary(avis['pmax'])
avis$PMAX = NULL
avis = avis %>% rename('PMAX' = 'pmax')

# mean_pmax = mean(avis$pmax, na.rm=TRUE)
# avis = avis %>%
#   mutate(PMAX = ifelse(is.na(pmax), mean_pmax, pmax))
# summary(avis['PMAX'])
# avis$pmax = NULL

avis = avis %>% group_by(CAI) %>%
  mutate(polso = ifelse(is.na(POLSO), mean(POLSO, na.rm=TRUE), POLSO))
summary(avis['polso'])

avis = avis %>% filter(is.na(polso) == FALSE)
summary(avis['polso'])
avis$POLSO = NULL
avis = avis %>% rename('POLSO' = 'polso')

# mean_polso = mean(avis$polso, na.rm=TRUE)
# avis = avis %>%
#   mutate(POLSO = ifelse(is.na(polso), mean_polso, polso))
# summary(avis['POLSO'])
# avis$polso = NULL

avis = avis %>% group_by(CAI) %>%
  mutate(emog = ifelse(is.na(EMOG), mean(EMOG, na.rm=TRUE), EMOG))
summary(avis['emog'])

avis = avis %>% filter(is.na(emog) == FALSE)
summary(avis['emog'])
avis$EMOG = NULL
avis = avis %>% rename('EMOG' = 'emog')

# mean_emog = mean(avis$emog, na.rm=TRUE)
# avis = avis %>%
#   mutate(EMOG = ifelse(is.na(emog), mean_emog, emog))
# summary(avis['EMOG'])
# avis$emog = NULL

# drop covariates that we don't use
avis$DTPRES = NULL
avis$PRIMA_DON = NULL
avis$ID_PRES = NULL
avis$CAP_DOMIC = NULL
avis$THE = NULL
avis$CAFFE = NULL
avis$DIETA = NULL
avis$STRESS = NULL
avis$time_prec = NULL
avis$gap_time = NULL
avis$eta_donaz = NULL

num_donaz = avis %>% group_by(CAI) %>% summarise(donazioni=n(), censoring=first(censoring))

avis$PESO = NULL
avis$ALTEZZA = NULL

# recode SESSO covariate as a dummy variable
avis$SESSO = as.factor(avis$SESSO)
levels(avis$SESSO)
avis$SESSO = fct_recode(avis$SESSO, '0' = '2')
avis$SESSO = as.numeric(as.character(avis$SESSO))

# make binary/dummy FUMO
avis$FUMO = as.factor(avis$FUMO)
LFUMO = levels(avis$FUMO)
avis$FUMO = fct_collapse(avis$FUMO, '1'=c(LFUMO[1:8], LFUMO[13]), '0'=LFUMO[9:12])
avis$FUMO = as.numeric(as.character(avis$FUMO))

# make binary/dummy ALCOOL
avis$ALCOOL = as.factor(avis$ALCOOL)
LALCOOL = levels(avis$ALCOOL)
avis$ALCOOL = fct_collapse(avis$ALCOOL, '1'=LALCOOL[1:4], '0'=LALCOOL[5:6])
avis$ALCOOL = as.numeric(as.character(avis$ALCOOL))

#make binary/dummy ATTIVITAFISICA
avis$ATTIVITAFISICA = as.factor(avis$ATTIVITAFISICA)
LATTIVITA = levels(avis$ATTIVITAFISICA)
avis$ATTIVITAFISICA = fct_collapse(avis$ATTIVITAFISICA, '1'=c(LATTIVITA[1:6], LATTIVITA[10:12]),
                                   '0'=c(LATTIVITA[7:9], LATTIVITA[13]))
avis$ATTIVITAFISICA = as.numeric(as.character(avis$ATTIVITAFISICA))

# make binary/dummy variables for each blood type
avis$AB0 = as.factor(avis$AB0)
levels(avis$AB0)
avis$TYPE_A = fct_collapse(avis$AB0, '1'='A', '0'=c('0','B','AB'))
avis$TYPE_A = as.numeric(as.character(avis$TYPE_A))
avis$TYPE_0 = fct_collapse(avis$AB0, '1'='0', '0'=c('A','B','AB'))
avis$TYPE_0 = as.numeric(as.character(avis$TYPE_0))
avis$TYPE_B = fct_collapse(avis$AB0, '1'='B', '0'=c('A','0','AB'))
avis$TYPE_B = as.numeric(as.character(avis$TYPE_B))
avis$TYPE_AB = fct_collapse(avis$AB0, '1'='AB', '0'=c('A','B','0'))
avis$TYPE_AB = as.numeric(as.character(avis$TYPE_AB))
avis$AB0 = NULL

# make binary/dummy RH
avis$RH = as.factor(avis$RH)
levels(avis$RH)
avis$RH = fct_recode(avis$RH, '1'='POS', '0'='NEG')
avis$RH = as.numeric(as.character(avis$RH))

donors = avis %>% group_by(CAI) %>%
         summarise(age_first=first(eta_prima), bmi=first(BMI),
                   pmax=first(PMAX), polso=first(POLSO))

# standardize
donors = donors %>% mutate(AGE_FIRST = (age_first - mean(age_first))/sd(age_first),
                           BMI = (bmi - mean(bmi))/sd(bmi),
                           PMAX = (pmax - mean(pmax))/sd(pmax),
                           POLSO = (polso - mean(polso))/sd(polso))

avis$PMIN = (avis$PMIN - mean(avis$PMIN))/sd(avis$PMIN)
avis$EMOG = (avis$EMOG - mean(avis$EMOG))/sd(avis$EMOG)


donors$age_first = NULL
donors$bmi = NULL
donors$polso = NULL
donors$pmax = NULL

# consider pmax and polso as constant. substitute values with first value
avis$eta_prima = NULL
avis$BMI = NULL
avis$POLSO = NULL
avis$PMAX = NULL

avis = left_join(avis, donors)

########    make donation times matrix     ###########
M = length(donors$CAI)
Nmax = max(num_donaz$donazioni)
tempi = avis[c('CAI', 'time')]
times = matrix(-1, nrow=M, ncol=Nmax+1)
for(i in 1:M){
  tempi_singolo = tempi %>% filter(CAI == num_donaz$CAI[i])
  for(j in 1:num_donaz$donazioni[i]){
    times[i,j] = as.numeric(tempi_singolo[j,2])
  }
  times[i, num_donaz$donazioni[i]+1] = num_donaz$censoring[i]
}

#######    make design array     ###########
avis = avis[c('CAI', 'time', 'censoring', 'SESSO', 'FUMO', 'ALCOOL', 'ATTIVITAFISICA',
              'TYPE_0', 'TYPE_A', 'TYPE_B', 'TYPE_AB', 'RH', 'AGE_FIRST', 'BMI',
              'POLSO', 'PMAX', 'EMOG', 'PMIN')]
P = 15
X = array(0, dim=c(M, Nmax, P))
for(i in 1:M){
  df_donor = avis %>% filter(CAI == num_donaz$CAI[i])
  n = num_donaz$donazioni[i]
  X[i, 1:n, 1:P] = array(as.matrix(df_donor[,4:18]))
}

#########    parameters and other data     ##########
# number of nodes
K = 11
nodes = seq(0, max(num_donaz$censoring), length.out=K)
# we need sex as an index and indices in R start from 1.
sex = avis %>% group_by(CAI) %>% summarise(sex=first(SESSO))
sex = sex$sex + 1
delay = c(150, 85)
a_eta = 3
b_eta = 2
a_lambda = 2
b_lambda = 2
sigma2_b = 100


########    STAN    #########
library(rstan)
options(mc.cores=2)

data = list(M=M,
            Nmax=Nmax,
            K=K,
            P=P,
            delay=delay,
            sex=sex,
            times=times,
            nodi=nodes,
            last=num_donaz$donazioni,
            X=X,
            a_eta=a_eta,
            a_lambda=a_lambda,
            b_eta=b_eta,
            b_lambda=b_lambda,
            sigma2_b=sigma2_b)

fit.all = stan(file='time-dependent.stan',
               data=data,
               chains=2,
               iter=20000,
               warmup=12000,
               thin=4,
               seed=42,
               algorithm="NUTS")

save(fit.all, file='fit-all.RData')

##########     DIAGNOSTICS - FIT ALL    #########
rm(list=ls())
load(file='fit_all.RData')

library(arulesViz)
library(coda)
names(fit.all)[11:25] = c('SESSO', 'FUMO', 'ALCOOL', 'ATTIVITAFISICA', 'TIPO_0', 'TIPO_A', 'TIPO_B',
                      'TIPO_AB', 'RH', 'ETA_PRIMA', 'BMI', 'POLSO', 'PMAX', 'EMOG', 'PMIN')


rstan::traceplot(fit.all, pars = "beta", inc_warmup = FALSE)
rstan::traceplot(fit.all, pars= "lambda", inc_warmup = FALSE)


coda_chain <- As.mcmc.list(fit.all, pars = c("beta", "lambda"))
summary(coda_chain)

gelman.diag(coda_chain, confidence = 0.95,  autoburnin = TRUE, multivariate=TRUE)
acfplot(coda_chain, lag.max = 30)

########      POSTERIOR ANALYSIS      ########       
plot_post <- fit.all %>% 
  rstan::extract("beta") %>% 
  as.data.frame() %>% 
  map_df(as_data_frame, .id = 'param')

plot_post %>% 
  ggplot(aes(value, fill = param)) + 
  geom_density() + 
  facet_wrap(~param, scales = 'free') + 
  theme_minimal() +
  theme(legend.position="none")

###########    Remove non significant covariates and fit again    ##########
avis$TYPE_0 = NULL
avis$TYPE_A = NULL
avis$TYPE_B = NULL
avis$TYPE_AB = NULL
avis$BMI = NULL
avis$PMAX = NULL
avis$ALCOOL = NULL
avis$ATTIVITAFISICA = NULL

# adding an intercept
avis$INTERCEPT = rep(1, times=dim(avis)[1])

# recompute design array
P = 8
X = array(0, dim=c(M, Nmax, P))
for(i in 1:M){
  df_donor = avis %>% filter(CAI == num_donaz$CAI[i])
  n = num_donaz$donazioni[i]
  X[i, 1:n, 1:P] = array(as.matrix(df_donor[,4:11]))
}


data = list(M=M,
            Nmax=Nmax,
            K=K,
            P=P,
            delay=delay,
            sex=sex,
            times=times,
            nodi=nodes,
            last=num_donaz$donazioni,
            X=X,
            a_eta=a_eta,
            a_lambda=a_lambda,
            b_eta=b_eta,
            b_lambda=b_lambda,
            sigma2_b=sigma2_b)

fit = stan(file = 'time-dependent.stan',
           data = data,
           chains = 2,
           iter = 24000,
           warmup = 12000,
           thin = 6,
           seed = 42,
           algorithm = "NUTS")

names(fit)[11:18] = c('SESSO', 'FUMO', 'RH', 'AGEFIRST', 'POLSO', 'EMOG', 'PMIN', 'INTERCEPT')

print(fit, 
      probs = c(0.025, 0.5, 0.975), 
      par = c('beta', 'lambda'))

plot(fit, pars = 'beta', col='blue')

rstan::traceplot(fit, pars = "beta", inc_warmup = FALSE)
rstan::traceplot(fit, pars= "lambda", inc_warmup = FALSE)


coda_chain <- As.mcmc.list(fit, pars = c("beta", "lambda"))
summary(coda_chain)

gelman.diag(coda_chain, confidence = 0.95,  autoburnin = TRUE, multivariate=TRUE)
acfplot(coda_chain, lag.max = 30)

########      POSTERIOR ANALYSIS      ########       
plot_post <- fit %>% 
  rstan::extract("beta") %>% 
  as.data.frame() %>% 
  map_df(as_data_frame, .id = 'param')

plot_post %>% 
  ggplot(aes(value, fill = param)) + 
  geom_density() + 
  facet_wrap(~param, scales = 'free') + 
  theme_minimal() +
  theme(legend.position="none")

save(fit, file='fit.RData')

WAIC <- function(fit, param){
  llik   <- rstan::extract(fit, param)[[1]]
  p_WAIC <- sum(apply(llik, 2, var))
  lppd   <- sum(apply(llik, 2, function(x) log(mean(exp(x)))))
  WAIC_score   <- - 2 * lppd + 2 * p_WAIC
  return(WAIC_score)
}

LPML = function(fit) {
  llik = rstan::extract(fit, 'log_lik')[[1]]
  CPO.inv = apply(llik, 2, function(x) mean(1/exp(x)))
  LPML_score = sum(log( 1/CPO.inv ))
  return(LPML_score)
}

WAIC_time.dep = WAIC(fit, 'log_lik')
LPML_time.dep = LPML(fit)

# time-fixed model to make comparisons
avis$PMIN = NULL
avis$POLSO = NULL
avis$EMOG = NULL

P = 5
X = array(0, dim=c(M, Nmax, P))
for(i in 1:M){
  df_donor = avis %>% filter(CAI == num_donaz$CAI[i])
  n = num_donaz$donazioni[i]
  X[i, 1:n, 1:P] = array(as.matrix(df_donor[,4:8]))
}

avis.df=avis
last=num_donaz$donazioni

# Save all the objects
save(avis.df, 
     M, 
     Nmax,
     K,
     P,
     last,
     delay,
     sex,
     times,
     nodes,
     last,
     X,
     a_eta,
     a_lambda,
     b_eta,
     b_lambda,
     sigma2_b,
     file = "avis_data_5.RData")

# To load the data again
load("avis_data_5.RData")




data = list(M=M,
            Nmax=Nmax,
            K=K,
            P=P,
            delay=delay,
            sex=sex,
            times=times,
            nodi=nodes,
            last=last,
            X=X,
            a_eta=a_eta,
            a_lambda=a_lambda,
            b_eta=b_eta,
            b_lambda=b_lambda,
            sigma2_b=sigma2_b)

fit.fixed = stan(file = 'time-dependent.stan',
           data = data,
           chains = 2,
           iter = 30000,
           warmup = 18000,
           thin = 6,
           seed = 42,
           algorithm = "NUTS")

save(fit.fixed, file='fit-fixed.RData')

names(fit.fixed)[11:15] = c('SESSO', 'FUMO', 'RH', 'AGEFIRST', 'INTERCEPT')

print(fit.fixed, 
      probs = c(0.025, 0.5, 0.975), 
      par = c('beta', 'lambda'))

plot(fit.fixed, pars = 'beta', col='blue')

rstan::traceplot(fit.fixed, pars = "beta", inc_warmup = FALSE)
rstan::traceplot(fit.fixed, pars= "lambda", inc_warmup = FALSE)

library('coda')

coda_chain <- As.mcmc.list(fit.fixed, pars = c("beta", "lambda"))
summary(coda_chain)

gelman.diag(coda_chain, confidence = 0.95,  autoburnin = TRUE, multivariate=TRUE)
x11()
acfplot(coda_chain, lag.max = 30,type='l')

########      POSTERIOR ANALYSIS      ########       
plot_post <- fit.fixed %>% 
  rstan::extract("beta") %>% 
  as.data.frame() %>% 
  map_df(as_data_frame, .id = 'param')

plot_post %>% 
  ggplot(aes(value, fill = param)) + 
  geom_density() + 
  facet_wrap(~param, scales = 'free') + 
  theme_minimal() +
  theme(legend.position="none")
