# r_propuesta: funcion que genera una propuesta
# d_propuesta: funcion de densidad de la distribucion de propuesta
# d_objetivo: funcion de densidad a partir de la cual se quieren obtener muestras 
#            (no necesariamente normalizada)
# punto_inicial: donde comienza el proceso de muestreo
# n: cantidad de muestras a obtener
sample_mh <- function(r_propuesta, d_propuesta, d_objetivo, punto_inicial, n) {
  x_dim <- length(punto_inicial)
  muestras <- matrix(NA, nrow = n, ncol = x_dim)
  muestras[1, ] <- punto_inicial
  for (i in 2:n) {
    muestra_actual <- muestras[i - 1, ]
    
    # Proponer un nuevo valor
    muestra_propuesta <- r_propuesta(muestra_actual)
    
    # Evaluar la funcion de densidad en el valor actual y en el propuesto
    p_propuesta <- d_objetivo(muestra_propuesta)
    p_actual <- d_objetivo(muestra_actual)
    
    # Evaluar la funcion de densidad de la propuesta en el valor actual y en
    # el propuesto.
    g_actual <- d_propuesta(muestra_actual, mean = muestra_propuesta)
    g_propuesta <- d_propuesta(muestra_propuesta, mean = muestra_actual)
    
    # Calcular probabilidad de aceptación
    alpha <- min(c(1, (p_propuesta / p_actual) * (g_actual / g_propuesta)))
    
    # Determinar aceptación de propuesta
    aceptar <- rbinom(1, 1, alpha)
    
    # Seleccionar nueva muestras 
    if (aceptar) {
      muestras[i, ] <- muestra_propuesta
    } else {
      muestras[i, ] <- muestra_actual
    }
  }
  if (x_dim == 1) {
    muestras <- as.vector(muestras)
  }
  return(muestras)
}