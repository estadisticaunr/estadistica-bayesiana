data {
  int<lower=1> N;             // Cantidad de observaciones
  int<lower=0, upper=1> y[N]; // Vector de respuesta (0 y 1)
  int partido_idx[N];         // Índice del partido
  vector[N] x;
}
parameters {
  vector[3] a;
  vector[3] b;
}
model {
  y ~ bernoulli_logit(a[partido_idx] + b[partido_idx] .* x);
}
