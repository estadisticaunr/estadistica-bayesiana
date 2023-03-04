set.seed(121195)
b0 <- 5
b1 <- 0.3
sigma <- 1.25
n <- 52
x_min <- 4
x_max <- 20
x <- runif(n, x_min, x_max)
y <- rnorm(n, b0 + b1 * x, sigma)

data <- data.frame(x = x , y = y )
readr::write_csv(data, here::here("data", "sales.csv"))