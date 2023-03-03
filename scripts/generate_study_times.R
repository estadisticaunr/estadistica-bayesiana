set.seed(1234)
n <- 20
mu <- 7.25
sd <- 1.2

estudiante <- seq(1, n)
horas <- rnorm(n, mu, sd)

df <- data.frame(estudiante = estudiante, horas = horas)
readr::write_csv(df, here::here("datos", "tiempo-estudio-eb.csv"))