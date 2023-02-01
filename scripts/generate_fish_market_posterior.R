# Datos para el Ejercicio 13 de Practica 3
library(brms)
library(dplyr)
datos <- readr::read_csv(here::here("datos", "fish-market.csv"))
datos <- datos[datos$Weight > 0, ]
modelo <- brm(log(Weight) ~ 1 + log(Length1), datos, seed = 121195, chains = 2)

df_posterior <- as_draws_df(modelo)
df_posterior <- df_posterior |>
  select(intercepto = b_Intercept, pendiente = b_logLength1, sigma)

readr::write_csv(df_posterior, here::here("datos", "fish-market-posterior.csv"))