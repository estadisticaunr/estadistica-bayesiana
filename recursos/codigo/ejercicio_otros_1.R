library(ggplot2)
library(ggdist)
library(dplyr)

url <- paste0(
  "https://raw.githubusercontent.com/estadisticaunr/",
  "estadistica-bayesiana/main/datos/fish-market-posterior.csv"
)

df_posterior <- readr::read_csv(url)

ggplot(df_posterior) +
  geom_histogram(aes(x=intercepto))

ggplot(df_posterior) +
  geom_histogram(aes(x=pendiente))

ggplot(df_posterior) +
  geom_histogram(aes(x=1.01^pendiente))

ggplot(df_posterior) +
  geom_histogram(aes(x=sigma))


ggplot(df_posterior) +
  geom_point(aes(x=intercepto, y=pendiente))


Largo <- 30

df_posterior$log_Peso_30_prom <- df_posterior$intercepto + df_posterior$pendiente * log(Largo) # log peso medio promedio

df_posterior$Peso_30_prom <- exp(df_posterior$log_Peso_30_prom) # peso medio promedio

ggplot(df_posterior) +
  geom_histogram(aes(x=Peso_30_prom))

df_posterior$log_Peso_30 <- rnorm(n = nrow(df_posterior), mean = df_posterior$log_Peso_30_prom, sd = df_posterior$sigma)

df_posterior$Peso_30 <- exp(df_posterior$log_Peso_30)

ggplot(df_posterior) +
  geom_histogram(aes(x=Peso_30))


log_largo <- seq(0,5,length.out=100)

calc_log_peso_prom <- function(x, posterior){
  return(posterior$intercepto + posterior$pendiente * x)
}

log_peso_prom <- sapply(log_largo, calc_log_peso_prom, df_posterior)

dim(log_peso_prom)

log_peso_prom_new <- 
  as.data.frame(t(log_peso_prom[200:299,])) |> 
  setNames(paste0("rep",1:100)) |>
  cbind(log_largo) |>
  tidyr::pivot_longer(cols = -log_largo, names_to = "rep", values_to = "value")

ggplot(log_peso_prom_new) +
  geom_line(aes(x=log_largo, y=value, group=rep), alpha=0.3)

ggplot(log_peso_prom_new) +
  geom_line(aes(x=exp(log_largo), y=exp(value), group=rep), alpha=0.3)

log_peso_prom_new_group <- 
log_peso_prom_new |>
  group_by(log_largo) |>
  summarise(q05 = quantile(value,0.05),
            mean = mean(value),
            q95 = quantile(value,0.95)) 

log_peso_prom_new_group |>
  ggplot() +
  geom_ribbon(aes(x = log_largo, ymin = q05, ymax = q95), fill="gray50") +
  geom_line(aes(x = log_largo, y = mean))

log_peso_prom_new_group |>
  ggplot() +
  geom_ribbon(aes(x = exp(log_largo), ymin = exp(q05), ymax = exp(q95)), fill="gray50") +
  geom_line(aes(x = exp(log_largo), y = exp(mean)))

log_peso_prom_new |>
  group_by(log_largo) |>
  mean_hdi(value, .width = c(.50, .70, .90),) |>
  ggplot(aes(x = log_largo, y = value, ymin = .lower, ymax = .upper)) +
  geom_lineribbon(size=.8) +
  scale_fill_brewer()

log_peso_prom_new |>
  group_by(exp_log_largo = exp(log_largo)) |>
  mean_hdi(exp_value = exp(value), .width = c(.50, .70, .90),) |>
  ggplot(aes(x = exp_log_largo, y = exp_value, ymin = .lower, ymax = .upper)) +
  geom_lineribbon(size=.8) +
  scale_fill_brewer()


pred_log_peso <- function(x, posterior){
  return(rnorm(n = nrow(posterior), mean = posterior$intercepto + posterior$pendiente * x, sd = posterior$sigma))
}

log_peso <- sapply(log_largo, pred_log_peso, df_posterior)

log_peso_new <- 
  as.data.frame(t(log_peso[600:699,])) |> 
  setNames(paste0("rep",1:100)) |>
  cbind(log_largo) |>
  tidyr::pivot_longer(cols = -log_largo, names_to = "rep", values_to = "value")

log_peso_new_group <- 
  log_peso_new |>
  group_by(log_largo) |>
  summarise(q05 = quantile(value,0.05),
            mean = mean(value),
            q95 = quantile(value,0.95)) 

log_peso_new_group |>
  ggplot() +
  geom_ribbon(aes(x = log_largo, ymin = q05, ymax = q95), fill="gray50") +
  geom_line(aes(x = log_largo, y = mean))

log_peso_new_group |>
  ggplot() +
  geom_ribbon(aes(x = exp(log_largo), ymin = exp(q05), ymax = exp(q95)), fill="gray50") +
  geom_line(aes(x = exp(log_largo), y = exp(mean)))
