data{
  int<lower=1> M; // numero individui 
  int<lower=1> Nmax; // numero massimo eventi individuo
  int<lower=2> K; // numero nodi 
  int<lower=0> P; // numero covariate - intero
  
  int<lower=0> delay[2];
  int<lower=1, upper=2> sex[M];
  matrix[M,Nmax+1] times; // tempi eventi (negativo se non avvenuta) + censoring time
  vector[K] nodi; // nodi
  int<lower=1> last[M]; // index of last observation
  
  // is the assignment from array[M,Nmax,P] to X correct?
  matrix[Nmax, P] X[M]; // design matrix/array
  
  real<lower=0> a_eta; // shape parameter eta
  real<lower=0> a_lambda; // shape parameter lambda_k
  real<lower=0> b_eta; // scale parameter eta
  real<lower=0> b_lambda; // scale parameter lambda_k
  real<lower=0> sigma2_b; // varianza beta
  }

transformed data{
 matrix[K-1,M] n;  // numero eventi in ogni intervallo di ogni individuo
 vector[K-1] npunto;  // numero di eventi totale per ogni intervallo
 vector[M] n_donor; // numero donazioni per individuo
 int nind[M];// numero eventi per ogni individuo
 matrix[Nmax, K-1] tau[M]; // K nodes and K-1 intervals
 int kappa[M, Nmax]; // indicate in which interval the donations take place
 
 
// inizializzo nind
 for(i in 1:M){
   nind[i] = 0;
   for(t in 1:(Nmax)){
     if(times[i,t]>=0) nind[i] +=1;
   }
 }
 
 // inizializzo n
 for(i in 1:M){
   
     for(k in 1:(K-1)){
       n[k,i] = 0;
     
     for(j in 1:nind[i]){
       
       if((times[i,j] > nodi[k]) && (times[i,j] <= nodi[k+1])){
          n[k,i] += 1;
          }
     }
   }
 }
 // inizializzo npunto
 for(k in 1:(K-1)) {
   npunto[k] = sum(n[k,]);
 }
 
 for(i in 1:M) {
   n_donor[i] = sum(n[,i]);
 }
 
 for(i in 1:M){
   for(j in 1:Nmax){
     for(k in 1:(K-1)){
       if(j <= last[i])
         tau[i,j,k] = fmax(fmin(times[i,j+1], nodi[k+1]) - fmax(times[i,j]+delay[sex[i]], nodi[k]), 0);
       else
         tau[i,j,k] = 0;
     }
   }
 }
 
 for(i in 1:M) {
   for(j in 1:last[i]) {
     for(k in 1:(K-1)) {
       if (nodi[k] < times[i,j] && times[i,j] <= nodi[k+1])
         kappa[i,j] = k;
     }
   }
 }
}

parameters {
  vector<lower=0>[K-1] lambda; // logaritmo valori dei rates
  vector[P] beta; // regression parameters
  vector<lower=0>[M] w; // log-frailties
  real<lower=0> eta; // variance of w[i] i=1,...,M
}

model{
  
 // log-likelihood processo 1a parte
 target += sum(npunto .* log(lambda)) + sum(n * log(w));
 
 for(i in 1:M){
   target += sum(X[i] * beta) - (w[i] * ((exp(X[i] * beta))' * tau[i] * lambda));
 }
 
 // log-priors
 target += gamma_lpdf(eta | a_eta, b_eta);
 target += gamma_lpdf(lambda | a_lambda, b_lambda);
 target += gamma_lpdf(w | 1/eta, 1/eta);
 target += normal_lpdf(beta | 0, sigma2_b);
}

generated quantities {
  vector[M] log_lik;
  for(i in 1:M) {
    log_lik[i] = 0;
    for(j in 1:last[i]) {
        log_lik[i] += log(lambda[kappa[i,j]]);
    }
    log_lik[i] += n_donor[i] * log(w[i]) +
                  sum(X[i] * beta) - (w[i] * ((exp(X[i] * beta))' * tau[i] * lambda));
  }
}
