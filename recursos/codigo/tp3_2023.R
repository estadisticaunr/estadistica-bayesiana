library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

theme_set(theme_bw() +
            theme(legend.position = "none",
                  panel.grid = element_blank(),
                  #strip.background = element_rect(fill="#ade658"),
                  text = element_text(size=26)))

calc_likelihood <- function(data, w0, w1, sd){
  lik_i <- double()
  lik_i <- dnorm(data$y, w0+w1*data$x, sd)
  return(prod(lik_i))
}

set.seed(610)

w0 <- -1
w1 <- 2
sd <- 0.8

data <- tibble(x = runif(50, -3, 3),
       y = rnorm(50,mean = w0 + w1*x, sd = 0.8))

ggplot(data) +
  geom_point(aes(x=x, y=y))


parameters <-
  expand.grid(w0 = seq(w0-3,w0+4,length.out = 100),
              w1 = seq(w1-5,w1+3,length.out = 100)) |>
  mutate(prior = purrr::map2_dbl(w0, w1, ~mvtnorm::dmvnorm(x = c(.x,.y), 
                                                           mean = c(0,0), 
                                                           sigma = diag(1,2)*0.5)))

# primer punto
dv1 <-
data %>% 
  slice(1:1) %>% 
  tidyr::crossing(parameters) %>% 
  nest(data = c(x,y)) %>%
  mutate(likelihood = purrr::pmap_dbl(list(data = data, w0 = w0, w1 = w1, sd = 0.8),
                         calc_likelihood)) %>%
  mutate(posterior = likelihood*prior) %>%
  select(-data)

prior <- 
  ggplot(data = dv1,
       aes(x=w0, y=w1, fill=prior)) +
  geom_raster() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  stat_contour(aes(x = w0, y = w1, z = prior), col = "white") +
  viridis::scale_fill_viridis()

likelihood <- 
  ggplot(data = dv1,
         aes(x=w0, y=w1, fill=likelihood)) +
  geom_raster() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  stat_contour(aes(x = w0, y = w1, z = likelihood), col = "white") +
  viridis::scale_fill_viridis()

posterior <- 
  ggplot(data = dv1,
         aes(x=w0, y=w1, fill=posterior)) +
  geom_raster() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  stat_contour(aes(x = w0, y = w1, z = posterior), col = "white") +
  viridis::scale_fill_viridis()

prior + likelihood + posterior 

# segundo punto
dv1 <-
  data %>% 
  slice(4:4) %>% 
  tidyr::crossing(parameters) %>% 
  nest(data = c(x,y)) %>%
  mutate(likelihood = purrr::pmap_dbl(list(data = data, w0 = w0, w1 = w1, sd = 0.8),
                                      calc_likelihood)) %>%
  mutate(posterior = likelihood*prior) %>%
  select(-data)

likelihood2 <- 
  ggplot(data = dv1,
         aes(x=w0, y=w1, fill=likelihood)) +
  geom_raster() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  stat_contour(aes(x = w0, y = w1, z = likelihood), col = "white") +
  viridis::scale_fill_viridis()

posterior2 <- 
  ggplot(data = dv1,
         aes(x=w0, y=w1, fill=posterior)) +
  geom_raster() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  stat_contour(aes(x = w0, y = w1, z = posterior), col = "white") +
  viridis::scale_fill_viridis()

(prior + likelihood + posterior) /
(posterior + likelihood2 + posterior2)

# dos puntos juntos
dv1 <-
  data %>% 
  slice(c(1,4)) %>% 
  tidyr::crossing(parameters) %>% 
  nest(data = c(x,y)) %>%
  mutate(likelihood = purrr::pmap_dbl(list(data = data, w0 = w0, w1 = w1, sd = 0.8),
                                      calc_likelihood)) %>%
  mutate(posterior = likelihood*prior) %>%
  select(-data)

prior <- 
  ggplot(data = dv1,
         aes(x=w0, y=w1, fill=prior)) +
  geom_raster() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  stat_contour(aes(x = w0, y = w1, z = prior), col = "white") +
  viridis::scale_fill_viridis()

likelihood <- 
  ggplot(data = dv1,
         aes(x=w0, y=w1, fill=likelihood)) +
  geom_raster() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  stat_contour(aes(x = w0, y = w1, z = likelihood), col = "white") +
  viridis::scale_fill_viridis()

posterior <- 
  ggplot(data = dv1,
         aes(x=w0, y=w1, fill=posterior)) +
  geom_raster() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  stat_contour(aes(x = w0, y = w1, z = posterior), col = "white") +
  viridis::scale_fill_viridis()

prior + likelihood + posterior 

library(dplyr)

polynom <- function(theta3, theta2, theta1, theta0, x){
  return(theta3*x^3 + theta2*x^2 + theta1*x^1 + theta0)
}

tibble(x = round(runif(120,-5,5),2),
       y = round(rnorm(120,polynom(-0.2, 0.6, 2.3, 0, x),3),2)) %>%
  ggplot() +
  geom_point(aes(x=x,y=y))

set.seed(612)
data <- tibble(x = round(runif(120,-5,5),2),
       y = round(rnorm(120,polynom(-0.2, 0.6, 2.3, 0, x),3),2))

write.csv(data[1:100,], file = "datos/tp3_train.csv", row.names = F)
write.csv(data[101:120,], file = "datos/tp3_test.csv", row.names = F)
  
