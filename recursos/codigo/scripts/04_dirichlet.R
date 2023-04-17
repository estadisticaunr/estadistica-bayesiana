# Instalar la libreria ggsimplex
# devtools::install_github('marvinschmitt/ggsimplex')
library(ggplot2)
library(ggsimplex)
library(gtools)

# Determinar vector de concentracion
alpha <- c(1.5, 1.5, 1.5)

# Generar valores aleatorios
rvs <- rdirichlet(200, alpha = alpha)

# Guardar valores aleatorios en un data frame
datos <- data.frame(rvs)
colnames(datos) <- c("x1", "x2", "x3")

# Por como funciona {ggsimplex} es necesario crear este tipo de columna especial
datos$pmp <- make_list_column(datos$x1, datos$x2, datos$x3)

# Graficar muestras
ggplot(datos) +
  coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
  theme_void() +
  geom_simplex_canvas() +
  geom_simplex_point(aes(pmp = pmp), size = 0.7, alpha = 0.8)


# Crear un data frame con los valores del vector de concentracion
# para luego graficar la funcion de densidad
df_dirichlet <- data.frame(dummy = 1)
df_dirichlet$Alpha <- list(alpha)

ggplot() +
  coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1))
  theme_void() +
  geom_simplex_canvas() +
  stat_simplex_density(
    data = df_dirichlet,
    fun = ddirichlet,
    args = alist(Alpha = Alpha)
  )


ggplot() +
  coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
  theme_void() +
  geom_simplex_canvas() +
  stat_simplex_density(
    data = df_dirichlet,
    fun = ddirichlet,
    args = alist(Alpha = Alpha)
  ) +
  geom_simplex_point(
    data = datos,
    aes(pmp = pmp),
    size = 0.7,
    alpha = 0.8
  )