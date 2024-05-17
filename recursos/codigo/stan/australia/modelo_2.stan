data {
  int<lower=0> N;       // Cantidad de observaciones
  int location_idx[N];  // Índice de la ciudad
  vector[N] temp3pm;    // Temperatura a las 3 pm (respuesta)
}
parameters {
  vector[2] beta0;      // Interceptos
  real<lower=0> sigma;  // Desvío estándar del error
}
model {
  beta0 ~ normal(20, 10);
  sigma ~ normal(0, 15);
  temp3pm ~ normal(beta0[location_idx], sigma);
}
generated quantities {
  vector[N] mu;
  vector[N] y_rep;
  vector[N] log_likelihood;
  
  // Calcular 'mu'
  mu = beta0[location_idx];

  for (i in 1:N) {
    // Obtención de muestras de la distribución predictiva a posteriori
    y_rep[i] = normal_rng(mu[i], sigma);

    // Cálculo de la log-verosimilitud
    log_likelihood[i] = normal_lpdf(temp3pm[i] | mu[i], sigma);
  }
}
