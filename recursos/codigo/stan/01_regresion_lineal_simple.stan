data {
  int<lower=0> N;  // Cantidad de observaciones
  vector[N] x;     // Valores de la variable predictora
  vector[N] y;     // Valores de la variable respuesta
}
parameters {
  real beta0;           // Intercepto
  real beta1;           // Pendiente
  real<lower=0> sigma;  // Desvio estándar del error
}
model {
  y ~ normal(beta0 + beta1 * x, sigma);
}
