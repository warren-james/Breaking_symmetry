data {
  
  int<lower=1> N; // number of data points

  int<lower=1> J; // number of students

  int<lower=1> L; // number of modules

  int<lower=1> K; // number of predictors

  vector<lower=0,upper=1>[N] y; // dependant variable
 
 matrix[N,K] X; // predictors

  int<lower=1, upper=J> student[N];

  int<lower=1, upper=L> module[N];

} 
parameters {

  vector[K] beta; //coefs for mu

  vector[K] gamma; // coefs for phi

  real<lower=0> sigma_a1; // student variance
 
 real<lower=0> sigma_a2; // student variance

  real<lower=0> sigma_m1; // module variance

  real<lower=0> sigma_m2; // module variance 

  vector[J] a1;

  vector[J] a2; 

  vector[L] m1;

  vector[L] m2;
} transformed parameters {

  vector<lower=0,upper=1>[N] mu;  // transformed linear predictor for mean of beta distribution

  vector<lower=0>[N] phi;         // transformed linear predictor for precision of beta distribution

  vector<lower=0>[N] A;           // parameter for beta distn

  vector<lower=0>[N] B;           // parameter for beta distn


  for (i in 1:N) {

      mu[i]  = inv_logit(X[i,] * beta + a1[student[i]] + m1[module[i]]); 
  
    phi[i] = exp(X[i,] * gamma + a2[student[i]] + m2[module[i]]); 
  
}


  A = mu .* phi;

  B = (1.0 - mu) .* phi;

} 
model {

  // // priors

  beta ~ normal(0, 2);

  gamma ~ normal(0, 1);

  sigma_a1 ~ cauchy(0, 10);

  sigma_a2 ~ cauchy(0, 10);

  sigma_m1 ~ cauchy(0, 10);

  sigma_m2 ~ cauchy(0, 10);

  // likelihood

  a1 ~ normal(0, sigma_a1);

  a2 ~ normal(0, sigma_a2);

  m1 ~ normal(0, sigma_m1);

  m2 ~ normal(0, sigma_m2);

  y ~ beta(A, B);

}
