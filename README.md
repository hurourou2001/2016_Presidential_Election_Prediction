**Overview**
This project utilizes Bayesian hierarchical modeling and Markov Chain Monte Carlo (MCMC) methods to analyze and predict the outcome of the 2016 U.S. 
Presidential Election based on public poll data. The primary tools and languages used include R, Stan, and various libraries such as tidyverse, mvtnorm, 
coda, lubridate, rstan, bayesplot, ggplot2, and reshape2.

**Project Structure**
Data Preparation: Filtering and transforming the dataset to focus on high-quality polls, calculating the number of respondents for each candidate, and sorting the data by poll end dates.
Stan Model Setup: Configuring and running a Bayesian hierarchical model using Stan to estimate the support rates for candidates.
MCMC Diagnostics: Utilizing MCMC diagnostics to ensure the convergence and quality of the model.
Metropolis-Hastings Algorithm: Implementing and running the Metropolis-Hastings algorithm for additional posterior sampling.
Visualization and Analysis: Visualizing the results and calculating the probability of different election outcomes based on the model.

**Skills and Algorithms**
Data Preparation and Cleaning
Libraries: tidyverse, lubridate, dplyr
Tasks: Data filtering, transformation, and sorting to prepare the dataset for modeling.
Bayesian Hierarchical Modeling
Library: rstan
Model: A hierarchical Bayesian model was specified to estimate the support rates for Trump, Clinton, and Johnson.
Parameters: Gamma and eta parameters for the beta distribution were estimated, transforming them into alpha and beta parameters.
Markov Chain Monte Carlo (MCMC)
Library: rstan, mvtnorm, coda
Algorithm: MCMC sampling using both Stan's NUTS sampler and a custom Metropolis-Hastings implementation.
Diagnostics: MCMC diagnostics such as trace plots, pair plots, and effective sample size calculations were used to ensure the model's convergence and accuracy.
