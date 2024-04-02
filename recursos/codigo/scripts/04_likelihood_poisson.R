# Cantidad de mensajes observados
library(ggplot2)

mensajes <- c(7, 3, 8, 9, 10, 12)

# Opción 1: Escribiendo la función de verosimilitud analíticamente
n <- length(mensajes)
total <- sum(mensajes)
producto_factoriales <- prod(factorial(mensajes))

# Grilla de valores para 'lambda'
# Teoréticamente, el soporte del parámetro es (0, infty). Lo acotamos en 20.
lambda <- seq(0, 20, length.out = 200)

# Cálculo de la verosimilitud
verosimilitud <- exp(-n * lambda) * lambda ^ total / producto_factoriales

# Visualización de la verosimilitud para los valores de lambda en la grilla
data.frame(x = lambda, y = verosimilitud) |>
    ggplot() +
    geom_line(aes(x = x, y = y), linewidth = 1) +
    labs(x = expression(lambda), y = expression("p(y | " ~ lambda ~ ")"))

# Opción 2: Utilizando 'dpois'.
# Para cada valor observado, se evalúa la pmf de la Poisson en una grilla
# de valores de lambda.
# Luego se multiplican los resultados de la pmf en los valores observados,
# para cada valor de lambda.
# Nota: Hay que verificar bien que R está reciclando los argumentos de
# la manera esperada
mensajes_matriz <- matrix(rep(mensajes, 200), nrow = 200, byrow = TRUE)
pmf <- dpois(mensajes_matriz, lambda)
verosimilitud <- apply(pmf, 1, prod)

data.frame(x = lambda, y = verosimilitud) |>
    ggplot() +
    geom_line(aes(x = x, y = y), linewidth = 1) +
    labs(x = expression(lambda), y = expression("p(y | " ~ lambda ~ ")"))