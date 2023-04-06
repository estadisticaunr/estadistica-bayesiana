# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Utilizando un prior elicitado grupalmente
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Determinar grilla de puntos
pi_grid <- seq(0, 1, length.out = 11)
print(pi_grid)

# Obtener prior
prior_ <- c(0, 1, 2, 2, 1, 0.5, 0.25, 0.125, 0.05, 0.005, 0)
prior <- prior_ / sum(prior_)

# Graficar prior
plot(
  pi_grid,
  prior,
  xlab = "pi",
  ylab = "probabilidad",
  main = "prior"
)
lines(pi_grid, prior)
axis(1, at = pi_grid)

# Recolectar datos
cantidad_de_rocklets <- 43 # n
cantidad_de_rocklets_azules <- 11 # y

# Calcular verosimilitud para cada valor de "pi" en la grilla
likelihood <- dbinom(
  cantidad_de_rocklets_azules, 
  cantidad_de_rocklets, 
  pi_grid
)

# Obtener posterior
posterior_ <- prior * likelihood
posterior <- posterior_ / sum(posterior_)

# Graficar posterior
plot(
  pi_grid, 
  posterior, 
  xlab = "pi", 
  ylab = "probabilidad",
  main = "posterior"
)
lines(pi_grid, posterior)
axis(1, at = pi_grid)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Utilizando un prior Beta
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Determinar grilla de puntos
pi_grid <- seq(0, 1, length.out = 50)

# Obtener prior
prior_ <- dbeta(pi_grid, 2.5, 10)
prior <- prior_ / sum(prior_)

# Graficar prior
plot(
  pi_grid, 
  prior, 
  xlab = "pi", 
  ylab = "probabilidad",
  main = "prior"
)
lines(pi_grid, prior)
axis(1, at = pi_grid)

# Calcular verosimilitud para cada valor de "pi"
likelihood <- dbinom(
  cantidad_de_rocklets_azules, 
  cantidad_de_rocklets, 
  pi_grid
)

# Obtener posterior 
posterior_ <- prior * likelihood
posterior <- posterior_ / sum(posterior_)

# Graficar posterior
plot(
  pi_grid, 
  posterior, 
  xlab = "pi", 
  ylab = "probabilidad",
  main = "posterior"
)
lines(pi_grid, posterior)
axis(1, at = pi_grid)

# Q: Que pasa con el prior si incrementamos el 'n'?
# Q: Que pasa con el posterior si incrementamos el 'n'?