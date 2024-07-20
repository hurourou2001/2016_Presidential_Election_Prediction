//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=0> J;
  int<lower=0> y[J];
  int<lower=0> n[J];
}

parameters {
real<lower=0, upper=1> theta[J];
real<lower=0, upper=1> gamma; // alpha/alpha+beta
real<lower=0, upper=1> eta; // 1/sqrt(alpha+beta)
}

transformed parameters{
real<lower=0> alpha;
real<lower=0> beta;
alpha = gamma/eta^2; // alpha and beta are transformed quantities,
beta = (1-gamma)/eta^2; // no need of Jacobian for sampling
}

model {
gamma ~ uniform(0, 1);
eta ~ uniform(0, 1);
theta ~ beta(alpha, beta);
for (j in 1:J)
y[j] ~ binomial(n[j], theta[j]);
}

