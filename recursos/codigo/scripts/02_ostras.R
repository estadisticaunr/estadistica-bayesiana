library(ggplot2)

# Valores observados de "Y"
y <- c(64, 13, 33, 18, 30, 20)

# Altura de la densidad a priori uniforme
prior <- rep(1 / 100, 400)

# Grilla con los valorse de lambda
lambda_grid <- seq(0, 100, length.out = 400)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Alternativa 1
# Replicar la formula de la función de verosimilitud en una función de R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Alternativa 1
f_likelihood <- function(lambda, y_vector) {
  n <- length(y_vector)
  y_suma <- sum(y_vector)
  denominador <- prod(factorial(y_vector))
  (exp(- n * lambda) * lambda ^ y_suma) / denominador
}

likelihood <- f_likelihood(lambda_grid, y)
posterior_ <- prior * likelihood
posterior_

# La densidad a posteriori no normalizada resulta `Inf` en algunos casos.
# Esto indica que el computo no es estable.
# Veamos otras alternativas.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Alternativa 2 
# Utilizar la funcion 'dpois' de R para evaluar la pdf en cada observación
# y luego obtener la función de verosimilitud multiplicando estos resultados.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

f_likelihood <- function(lambda, y_vector) {
  output <- numeric(length(lambda))
  for (i in seq_along(output)) {
    output[i] <- prod(dpois(y_vector, lambda[i]))
  }
  output
}

likelihood <- f_likelihood(lambda_grid, y)
posterior_ <- prior * likelihood
area <- integrate(f_likelihood, lower = 0, upper = 100, y_vector = y)$value
posterior <- posterior_ / area

df <- data.frame(
  grupo = factor(
    rep(c("prior", "likelihood", "posterior"), each = 400), 
    levels = c("prior", "likelihood", "posterior"),
    ordered = TRUE
  ),
  lambda = rep(lambda_grid, 3),
  valor = c(prior, likelihood, posterior)
)

ggplot(df) +
  geom_line(aes(x = lambda, y = valor)) +
  facet_wrap(~ grupo, scales = "free_y")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Alternativa 3 
# Utilizar la funcion 'dpois' de R, pero hacer cuentas en escalas logaritmica
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

f_likelihood <- function(lambda, y_vector) {
  output <- numeric(length(lambda))
  for (i in seq_along(output)) {
    # El producto se transforma en suma
    output[i] <- sum(dpois(y_vector, lambda[i], log = TRUE))
  }
  # Volver a la escala original
  exp(output)
} 

likelihood <- f_likelihood(lambda_grid, y)
posterior_ <- prior * likelihood
area <- integrate(f_likelihood, lower = 0, upper = 100, y_vector = y)$value
posterior <- posterior_ / area

df <- data.frame(
  grupo = factor(
    rep(c("prior", "likelihood", "posterior"), each = 400), 
    levels = c("prior", "likelihood", "posterior"),
    ordered = TRUE
  ),
  lambda = rep(lambda_grid, 3),
  valor = c(prior, likelihood, posterior)
)

ggplot(df) +
  geom_line(aes(x = lambda, y = valor)) +
  facet_wrap(~ grupo, scales = "free_y")


