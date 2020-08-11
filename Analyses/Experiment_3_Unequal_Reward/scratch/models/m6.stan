data {
  int<lower = 0> N; // No. data points
  vector<lower = 0, upper = 1>[N] Norm_Dist;
  int<lower = 0, upper = 1> Unequal[N];
  vector<lower = 0, upper = 1>[N] Delta;
}
parameters {
  real mu_D;  // coef for Delta
  real mu_U;  // coef for Unequal
  real mu_DU; // coef for interaction
  real<lower = 0> gamma_D;
  real<lower = 0> gamma_U;
  real<lower = 0> gamma_DU;
}
transformed parameters {
  vector<lower = 0, upper = 1>[N] mu; // transformed predictor of mean 
  vector<lower = 0>[N] phi;           // transformed predictor for precision

  vector<lower = 0>[N] A;   // parameter for beta dist 
  vector<lower = 0>[N] B;   // parameter for beta dist

  for(n in 1:N){
      mu[n] = inv_logit(mu_D * Delta[n] +
                        mu_U * Unequal[n] +
                        mu_DU * Delta[n] * Unequal[n]);
      phi[n] = exp(gamma_D * Delta[n] +
                   gamma_U * Unequal[n] +
                   gamma_DU * Delta[n] * Unequal[n]);
  }

  A = mu .* phi;
  B = (1.0 - mu) .* phi;
}
model {
  mu_D ~ normal(0,2);
  mu_U ~ normal(0,2);
  mu_DU ~ normal(0,2);
  gamma_D ~ normal(0,2);
  gamma_U ~ normal(0,2);
  gamma_DU ~ normal(0,2);

  Norm_Dist ~ beta(A, B);
}
