data {
  int<lower=0> N;  // Tamaño de muestra
  vector[N] x;     // Valores del predictor
  vector[N] y;     // Valores de la respuesta
  real beta0_mu;   // Media del prior del intercepto
}
parameters {
  // Primero se declaran los parámetros
  real beta0;
  real beta1;
  real<lower=0> sigma;
}
model {
  // Luego se especifican sus priors
  beta0 ~ normal(beta0_mu, 10);  // Acá se pasa el valor de la media
  beta1 ~ normal(0, 0.5);
  sigma ~ normal(0, 5);          // Es una 'media-normal' dada la cota inferior arriba
  // Likelihood
  y ~ normal(beta0 + beta1 * x, sigma);
}
