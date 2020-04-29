data {
  int N; // sample size
  int P; // number of design parameters
  
  vector[N   ] y; // outcome data
  matrix[N, P] X; // design matrix
    
  real pSd; // informative prior SD
}

transformed data {
  // create a constant vector to estimate intercepts:
  vector[N] k;
  k = rep_vector(1.0, N);
}

parameters {
  real      alpha; // measurement intercepts
  vector[P] beta;  // regression slopes
  real      sigma; // residual variance
}

transformed parameters {
  vector[N] mu; // model-implied outcomes

  mu = k * alpha + X * beta; // model for outcome means
}

model {  
  alpha ~ normal(0.0, pSd);
  beta  ~ normal(0.0, pSd);

  for(n in 1 : N) y[n] ~ normal(mu[n], sigma);
}
