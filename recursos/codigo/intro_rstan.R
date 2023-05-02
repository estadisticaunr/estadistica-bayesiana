library(rstan)
library(tidybayes)
library(ggdist)

N <- 20
y <- 4

model_beta1_stan <- "
data {
  int N;     
  int Y; 
}
parameters {
  real<lower=0, upper=1> pi;
}
model {
  pi ~ beta(2,2); // prior
  Y ~ binomial(N, pi);  // likelihood
}"

# No olvidar el final de línea ;
# Todas las variables tienen que estar declaradas

model_beta1 <- stan_model(model_code = model_beta1_stan)

data_list <- list(Y=y, N=N)

model_beta1_fit <- sampling(object=model_beta1, 
                            data=data_list, 
                            chains = 3, 
                            iter = 1000,
                            warmup = 100)

model_beta1_fit

model_beta1_fit@model_pars

list_of_draws <- extract(model_beta1_fit,pars="pi")
str(list_of_draws)

dim(list_of_draws$theta)

head(list_of_draws$theta)

ggplot2::qplot(list_of_draws$theta)

mean(list_of_draws$theta<0.6)

bayesplot::mcmc_trace(array_of_draws,pars="pi")

bayesplot::mcmc_hist_by_chain(array_of_draws,pars="pi")

bayesplot::mcmc_dens_chains(array_of_draws,pars="pi")

df_of_draws <- as.data.frame(model_beta1_fit,pars="pi")

model_beta1_fit |>
  spread_draws(pi) |>
  ggplot(aes(x=pi)) + 
  stat_histinterval()

