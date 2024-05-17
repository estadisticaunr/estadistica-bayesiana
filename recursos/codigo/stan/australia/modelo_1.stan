data {
  int<lower=0> N;     // Cantidad de observaciones
  vector[N] temp9am;  // Temperatura a las 9 am (predictor)
  vector[N] temp3pm;  // Temperatura a las 3 pm (respuesta)
}
parameters {
  real beta0;           // Intercepto
  real beta1;           // Pendiente
  real<lower=0> sigma;  // Desvío estándar del error
}
model {
  beta0 ~ normal(15, 8);
  beta1 ~ normal(0, 3);
  sigma ~ normal(0, 12);
  temp3pm ~ normal(beta0 + beta1 * temp9am, sigma);
}
generated quantities {
  vector[N] mu;
  vector[N] y_rep;
  vector[N] log_likelihood;
  
  // Calcular 'mu'
  mu = beta0 + beta1 * temp9am;

  for (i in 1:N) {
    // Obtención de muestras de la distribución predictiva a posteriori
    y_rep[i] = normal_rng(mu[i], sigma);

    // Cálculo de la log-verosimilitud
    log_likelihood[i] = normal_lpdf(temp3pm[i] | mu[i], sigma);
  }
}
