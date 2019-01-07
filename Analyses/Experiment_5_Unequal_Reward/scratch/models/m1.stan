data {
  int<lower = 0> N; 
  vector[N] Norm_Dist;
  vector[N] Delta; 
}
parameters {
  real a;       // Intercept
  real b_delta; // effect of Delta
  real<lower = 0> sigma;
}
model {
  a ~ normal(0,10);
  b_delta ~ normal(1,2);
  sigma ~ cauchy(0,5);
  Norm_Dist ~ normal(Delta * b_delta + a, sigma);
}
