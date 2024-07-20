
```{r}
library(tidyverse)
library(mvtnorm)
library(coda)
library(lubridate)
library(dplyr)
library(rstan)
library(bayesplot)
library(ggplot2)
library(dplyr)
library(reshape2)
election = read.csv("presidential_polls.csv")
ele = election %>% 
  filter(state == "U.S.", grade == c("A+", "A", "A-"), adjpoll_johnson != "NA") %>%
  mutate(num_trump = samplesize * adjpoll_trump / 100 , 
         num_clinton = samplesize * adjpoll_clinton /100, 
         num_johnson = samplesize * adjpoll_johnson /100, 
         other_num = samplesize - num_trump - num_clinton - num_johnson) %>% 
  arrange(mdy(enddate))
#Condense dataset, select the sample that has the greatest poll_wt across U.S.


ele_new = ele%>%arrange(desc(mdy(enddate)))
```
```{r}
rstan_options(auto_wirte = T)
options(mc.cores = parallel::detectCores())
dat = list(
J = 230,
y = as.vector(as.integer(ele$num_trump)),
n = as.vector(ele$samplesize)
)
```
```{r}
fit =  stan(file = "451final.stan", data=dat, thin=3, seed=123, 
            iter=10000, chains=4)
```

```{r}
posterior <- as.array(fit)
df_posterior <- as.data.frame(fit)
np <- nuts_params(fit)
mcmc_pairs(posterior, np = np, pars = c("alpha","beta","lp__"),
           off_diag_args = list(size = 0.75))
mcmc_trace(posterior, pars = "alpha", np = np) +
xlab("Post-warmup iteration")
mcmc_trace(posterior, pars = "beta", np = np) +
xlab("Post-warmup iteration")
mcmc_trace(posterior, pars = "theta[32]", np = np) +
xlab("Post-warmup iteration")
```


```{r}
mcmc_neff(neff_ratio(fit)) + yaxis_text(hjust = 1)
df_posterior$theta230 <- df_posterior$`theta[230]`
df_posterior$postmean230 <- ((df_posterior$alpha + dat$y[230])
/ (df_posterior$beta + df_posterior$alpha + dat$n[230]))
sub_df <- df_posterior %>% select(theta230, postmean230) %>% melt()
ggplot(sub_df)+geom_histogram(binwidth=0.0005, aes(x=value, fill=variable)) +
geom_vline(xintercept=4/14) +
scale_fill_brewer(palette = "Paired", direction=-1)

```

```{r}

posterior <- as.array(fit)
df_posterior <- as.data.frame(fit)
df_posterior$theta230 <- df_posterior$theta[230]
df_posterior$postmean230 <- ((df_posterior$alpha + dat$y[230])
/ (df_posterior$beta + df_posterior$alpha + dat$n[230]))
sub_df <- df_posterior %>% select(theta230, postmean230) %>% melt()
ggplot(sub_df) + geom_histogram(binwidth=0.005, aes(x=value, fill=variable)) +
geom_vline(xintercept=4/14) +
scale_fill_brewer(palette = "Paired", direction=-1)
```


\
We can see from the dataset, prior distribution on (theta1, theta2, theta3, theta4) = (518, 544, 75, 84)
\
Prior: We assume a uninformative prior distribution Dirichlet distribution with alpha1 = alpha2 = alpha3 = alpha4 = 1. Therefore, we can easily obtain the posterior distribution ~ Dir(519, 544, 75, 84)

```{r}
LL <- function(theta, y){
  theta4 = 1 - theta[1] - theta[2] - theta[3]
  if(theta[1]+theta[2]+theta[3] > 1 | theta[1] < 0 | theta[2]<0 | theta[3]<0){
    return(-Inf)
  } else{
    lprior <- 0
    ll <- y[1] * log(theta[1]) + y[2] * log(theta[2]) + y[3] * log(theta[3]) + y[4] * log(theta4)
    lposterior <- lprior + ll
    return (lposterior)
  }
}


```
```{r}
mcmc_MH <- function(Niter, proposal_sigma, theta_init, y, LL){
  theta_new <- theta_init
  d <- length(theta_new)
  samples_MH <- theta_new
  accept_MH <- 0
  for(iter in 1: Niter){
    if(d > 1){
      propose_MH_theta <- as.numeric(
      rmvnorm(1, mean = theta_new, sigma = diag(rep(proposal_sigma^2, d))))
    }
    if(d == 1){
      propose_MH_theta <- rnorm(1, mean = theta_new, sd = proposal_sigma)
    }
    rtemp <- LL(propose_MH_theta, y) - LL(theta_new, y)
    if(runif(1) < exp(rtemp)) {
      theta_new <- propose_MH_theta
      accept_MH <- accept_MH + 1
    }
    samples_MH <- rbind(samples_MH, theta_new)
  }
  return(list(samples = samples_MH, acceptance = accept_MH /Niter))
}
```
```{r}
set.seed(1000)
Niter = 20000
burnIn = 0.2 * Niter
proposal_sigma = 0.02 # Set sigma to 0.02
theta_init = c(0.3, 0.4, 0.2) # Set theta initial
y = c(519, 545, 76, 85)
samplesMH = mcmc_MH(Niter, proposal_sigma, theta_init, y, LL)
plot(apply(samplesMH$samples, 2, jitter), type = 'l', xlab = '', ylab = '')
points(t(theta_init), cex = 2, col = 'red')

```
```{r}
#Use the latest poll to calculate the probability Trump wins.
n_new = 20000
n_new_bi = 0.2 * n_new
y_new = c(ele_new$num_trump[1],ele_new$num_clinton[1], ele_new$num_johnson[1], ele_new$other_num[1])
  samplesMH_new = mcmc_MH(n_new, proposal_sigma, theta_init, y_new, LL)
  samples_burnIn_new = samplesMH_new$samples[n_new_bi:n_new, ]
  #The probability that Trump wins
  mean(samples_burnIn_new[,1] > samples_burnIn_new[,2])
```


```{r}
trump_win_prob = rep(0, 230)
n = 2000
new_burn_in = 0.2 * n
for (i in 1:230) { #loop every row in the filtered dataset
  y = c(ele$num_trump[i], ele$num_clinton[i], ele$num_johnson[i], ele$other_num[i])
  samplesMH1 = mcmc_MH(n, proposal_sigma, theta_init, y, LL)
  samples_burnIn1 = samplesMH1$samples[new_burn_in:n, ]
  #The probability that Trump wins
  trump_win_prob[i] = mean(samples_burnIn1[,1] > samples_burnIn1[,2])
}
```
```{r}
plot(trump_win_prob)
print(paste("Acceptance rate is", samplesMH$acceptance))
```
```{r}
par(mfrow = c(2, 3))
for(k in 1:2){
plot(jitter(samplesMH$samples[, k]), type = 'l', xlab = '', ylab = '')
hist(samplesMH$samples[, k], xlab = '', main = '', freq = FALSE)
acf(samplesMH$samples[, k])
print(paste("Effective sample size", k, " is", effectiveSize(samplesMH$samples[, k])))
}
```
```{r}
#Use the most-poll_wt-sample to estimate the prob of Trump wins.
samples_burnIn = samplesMH$samples[burnIn:Niter, ]
#The probability that Trump wins
mean(samples_burnIn[,1] > samples_burnIn[,2]) 
```

