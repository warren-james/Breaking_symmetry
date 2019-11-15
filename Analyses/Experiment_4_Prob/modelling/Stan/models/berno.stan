data {

  int<lower = 1> N;                // No. data points 
  int<lower = 1> K;                // No. predictors
  int<lower = 0, upper = 1> y[N];  // dependent variable
  matrix[N,K] X;                   // Matrix of predictors
 
} parameters {

  vector[K] beta ;  // matrix of coefs

} transformed parameters {

} model {

  y ~ bernoulli_logit(X * beta);

}
