data {
  int<lower = 0> N; 
  vector[N] Norm_Dist;
  int<lower = 0, upper = 1> Unequal[N]; 
  vector[N] Delta; 
}
parameters {
  real a;         // Intercept
  real b_delta;   // effect of Delta
  real b_unequal; // effect of gamble type
  real<lower = 0> sigma;
}
model {
  a ~ normal(0,10);
  b_delta ~ normal(0,2);
  b_unequal ~ normal(0,2);
  sigma ~ cauchy(0,5);

  for(n in 1:N)
      Norm_Dist ~ normal(Delta[n] * b_delta + Unequal[n] * b_unequal + a, sigma);
}
