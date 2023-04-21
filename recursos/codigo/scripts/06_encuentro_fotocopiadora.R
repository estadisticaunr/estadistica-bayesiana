library(ggplot2)

# Tiempo que espera cada estudiante
espera_a <- 10 
espera_b <- 14

# Dos tiempos de llegada posible para cada estudiante
llegada_a <- runif(1, min = 0, max = 60)
llegada_b <- runif(1, min = 0, max = 60)

# El intervalo en el que cada estudiante está en la fotocopiadora
intervalo_a <- c(llegada_a, llegada_a + espera_a)
intervalo_b <- c(llegada_b, llegada_b + espera_b)

# Lo convertimos a data frame para graficar con ggplot2
datos <- data.frame(
  intervalo = c(intervalo_a, intervalo_b),
  persona = rep(c("A", "B"), each = 2)
)

# Graficamos con ggplot2
ggplot(datos) +
  geom_line(
    aes(x = intervalo, y = persona, color = persona), 
    linewidth = 2
  ) +
  xlim(c(0, 75))

# Si los intervalos se solapan, significa que hay una linea vertical que
# cruza a ambos. Esto es lo mismo que decir que hay una C que cumple:
# a1 <= C <= a2
# b1 <= C <= b2

# Y ambas se cumplen cuando...
# a1 <= b2 y b1 <= a2 
# Es lo mismo que decir que...
# A llega antes de que B se vaya, y B llega antes de que A se vaya
(intervalo_a[1] <= intervalo_b[2]) & (intervalo_b[1] <= intervalo_a[2])

# Lo extendemos a muchas iteraciones...
llegada_a <- runif(10000, min = 0, max = 60)
llegada_b <- runif(10000, min = 0, max = 60)

comparaciones <- (
  (llegada_a <= llegada_b + espera_b)   # A llega antes que B se vaya
  & (llegada_b <= llegada_a + espera_a) # B llega antes que A se vaya
)
mean(comparaciones)