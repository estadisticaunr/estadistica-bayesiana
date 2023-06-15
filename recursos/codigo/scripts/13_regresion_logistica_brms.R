library(brms)
library(ggplot2)
library(dplyr)

url <- "https://raw.githubusercontent.com/estadisticaunr/estadistica-bayesiana/main/datos/elecciones.csv"
datos <- readr::read_csv(url)

tabla <- table(datos$voto, datos$partido)

# Distribucion conjunta
prop.table(tabla)

# Distribucion marginal 1
# Como se componen los votos de cada candidato en terminos de las afinidades partidarias
prop.table(tabla, margin = 1) |> round(2)

# Distribucion marginal 2
# Como se distribuyen los votos de cada partido a los candidatos
prop.table(tabla, margin = 2) |> round(2)

# Creamos una variable indicadora para estar seguros que 'candidato A' es el éxito
datos$y <- ifelse(datos$voto == "candidato A", 1, 0)
modelo_1 <- brm(y ~ 1 + edad, datos, family = "bernoulli", refresh = 0)
modelo_2 <- brm(y ~ 0 + partido + edad, datos, family = "bernoulli", refresh = 0)
modelo_3 <- brm(y ~ 0 + partido + edad:partido, datos, family = "bernoulli", refresh = 0)


df_draws_1 <- as.data.frame(modelo_1$fit) |> select(-lprior, -lp__)
df_draws_2 <- as.data.frame(modelo_2$fit) |> select(-lprior, -lp__)
df_draws_3 <- as.data.frame(modelo_3$fit) |> select(-lprior, -lp__)


edad_min <- min(datos$edad)
edad_max <- max(datos$edad)
edad_grid <- seq(edad_min, edad_max, length.out = 100)
partidos <- unique(datos$partido)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modelo 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(modelo_1)

df_draws_1 |>
  tidyr::pivot_longer(
    everything(), 
    names_to = "coeficiente",
    values_to = "valor"
  ) |>
  ggplot() +
  geom_density(aes(x = valor, color = coeficiente, fill = coeficiente)) +
  scale_color_manual(values = c("#3498db", "#e74c3c")) +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +
  facet_wrap(~ coeficiente, scales = "free") +
  theme(legend.position = "none")

df_new <- data.frame(edad = edad_grid)
df_mean_1 <- data.frame(
  p = as.vector(posterior_epred(modelo_1, newdata = df_new)),
  edad = rep(edad_grid, each = 4000),
  draw = rep(1:4000, times = length(edad_grid))
)

ggplot(df_mean_1[df_mean_1$draw %in% sample(4000, 100), ]) +
  geom_line(
    aes(x = edad, y = p, group = draw), 
    alpha = 0.6, 
    color = "#3498db"
  ) + 
  geom_line(
    aes(x = edad, y = p),
    color = "grey20",
    linewidth = 1,
    data = df_mean_1 |> group_by(edad) |> summarise(p = mean(p))
  )
  labs(y = "P(voto = Candidato A)")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modelo 2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(modelo_2)

df_draws_2_long <- df_draws_2 |>
  tidyr::pivot_longer(
    everything(), 
    names_to = "coeficiente",
    values_to = "valor"
  ) |>
  mutate(
    grupo = case_when(
      coeficiente == "b_partidoazul" ~ "intercepto",
      coeficiente == "b_partidorojo" ~ "intercepto",
      coeficiente == "b_partidoverde" ~ "intercepto",
      coeficiente == "b_edad" ~ "pendiente"
    ),
    partido = case_when(
      coeficiente == "b_partidoazul" ~ "azul",
      coeficiente == "b_partidorojo" ~ "rojo",
      coeficiente == "b_partidoverde" ~ "verde",
      coeficiente == "b_edad" ~ "global"
    )
  ) |>
  mutate(
    partido = factor(
      partido, levels = c("azul", "rojo", "verde", "global"), ordered = TRUE
    )
  )

head(df_draws_2_long)

ggplot(df_draws_2_long) +
  geom_density(aes(x = valor, color = partido, fill = partido), alpha = 0.5) +
  scale_color_manual(
    values = c("#3498db", "#e74c3c","#2ecc71", "#535c68"), 
    name = "Partido"
  ) +
  scale_fill_manual(
    values = c("#3498db", "#e74c3c","#2ecc71", "#535c68"),
    name = "Partido"
  ) +
  facet_wrap(~ grupo, scales = "free") +
  theme(legend.position = "top")


df_new <- data.frame(
  edad = rep(edad_grid, 3), 
  partido = rep(partidos, each = length(edad_grid))
)

df_mean_2 <- data.frame(
  p = as.vector(posterior_epred(modelo_2, newdata = df_new)),
  edad = rep(rep(edad_grid, each = 4000), times = 3),
  partido = rep(partidos, each = 4000 * length(edad_grid)),
  draw = rep(1:4000, times = length(edad_grid) * length(partidos))
)

ggplot(df_mean_2[df_mean_2$draw %in% sample(4000, 100), ]) +
  geom_line(
    aes(
      x = edad, 
      y = p, 
      group = interaction(draw, partido), 
      color = partido
    ), 
    alpha = 0.5
  ) + 
  geom_line(
    aes(x = edad, y = p, group = partido),
    linewidth = 0.7,
    color = "grey20",
    data = df_mean_2 |> group_by(edad, partido) |> summarise(p = mean(p))
  ) + 
  scale_color_manual(
    values = c("#3498db", "#e74c3c","#2ecc71", "#535c68"), 
    name = "Partido"
  ) +
  labs(y = "P(voto = Candidato A)") +
  theme(legend.position = "top")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modelo 3
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(modelo_3)

df_draws_3_long <- df_draws_3 |>
  tidyr::pivot_longer(
    everything(), 
    names_to = "coeficiente",
    values_to = "valor"
  ) |>
  mutate(
    grupo = case_when(
      coeficiente == "b_partidoazul" ~ "intercepto",
      coeficiente == "b_partidorojo" ~ "intercepto",
      coeficiente == "b_partidoverde" ~ "intercepto",
      coeficiente == "b_partidoazul:edad" ~ "pendiente",
      coeficiente == "b_partidorojo:edad" ~ "pendiente",
      coeficiente == "b_partidoverde:edad" ~ "pendiente"
    ),
    partido = case_when(
      coeficiente %in% c("b_partidoazul", "b_partidoazul:edad")~ "azul",
      coeficiente %in% c("b_partidorojo", "b_partidorojo:edad") ~ "rojo",
      coeficiente %in% c("b_partidoverde", "b_partidoverde:edad") ~ "verde",
    )
  ) |>
  mutate(
    partido = factor(
      partido, levels = c("azul", "rojo", "verde", "global"), ordered = TRUE
    )
  )

head(df_draws_3_long)

ggplot(df_draws_3_long) +
  geom_density(aes(x = valor, color = partido, fill = partido), alpha = 0.5) +
  scale_color_manual(
    values = c("#3498db", "#e74c3c","#2ecc71"), 
    name = "Partido"
  ) +
  scale_fill_manual(
    values = c("#3498db", "#e74c3c", "#2ecc71"),
    name = "Partido"
  ) +
  facet_wrap(~ grupo, scales = "free") +
  theme(legend.position = "top")


df_new <- data.frame(
  edad = rep(edad_grid, 3), 
  partido = rep(partidos, each = length(edad_grid))
)

df_mean_3 <- data.frame(
  p = as.vector(posterior_epred(modelo_3, newdata = df_new)),
  edad = rep(rep(edad_grid, each = 4000), times = 3),
  partido = rep(partidos, each = 4000 * length(edad_grid)),
  draw = rep(1:4000, times = length(edad_grid) * length(partidos))
)

ggplot(df_mean_3[df_mean_3$draw %in% sample(4000, 100), ]) +
  geom_line(
    aes(
      x = edad, 
      y = p, 
      group = interaction(draw, partido), 
      color = partido
    ), 
    alpha = 0.5
  ) + 
  geom_line(
    aes(x = edad, y = p, group = partido),
    linewidth = 0.7,
    color = "grey20",
    data = df_mean_3 |> group_by(edad, partido) |> summarise(p = mean(p))
  ) + 
  scale_color_manual(
    values = c("#3498db", "#e74c3c","#2ecc71", "#535c68"), 
    name = "Partido"
  ) +
  labs(y = "P(voto = Candidato A)") +
  theme(legend.position = "top")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Algunos calculos de probabilidades...

mean(df_draws_3["b_partidoazul:edad"] > df_draws_3["b_partidorojo:edad"])
mean(df_draws_3["b_partidoazul:edad"] > df_draws_3["b_partidoverde:edad"])
mean(df_draws_3["b_partidoverde:edad"] > df_draws_3["b_partidorojo:edad"])

mean(df_draws_3["b_partidoazul:edad"] > 0)
mean(df_draws_3["b_partidorojo:edad"] < 0)
mean(df_draws_3["b_partidoverde:edad"] < 0)

# Comparando los modelos con LOO... ¿qué significa?
loo_1 <- loo(modelo_1)
loo_2 <- loo(modelo_2)
loo_3 <- loo(modelo_3)

loo_compare(loo_1, loo_2, loo_3)