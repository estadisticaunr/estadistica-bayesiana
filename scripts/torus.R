library(ggplot2)
library(patchwork)
vector_norm <- function(x) sqrt(sum(x^2))

scales <- c(0.5, 0.2, 0.05, 0.01)
plot_list <- vector("list", length(scales))

for (i in seq_along(scales)) {
  scale <- scales[[i]]
  grid <- seq(-2, 2, length.out = 500)
  df_mesh <- expand.grid(grid, grid)
  df_mesh$density <- exp(-((1 - apply(df_mesh, 1, vector_norm)) / scale) ^ 2)
  colnames(df_mesh) <- c("x", "y", "density")

  plt <- ggplot(df_mesh, aes(x = x, y = y)) +
    geom_raster(aes(fill = density)) +
    viridis::scale_fill_viridis() +
    theme(legend.position = "none")

  plot_list[[i]] <- plt
}

fig <- Reduce(`+`, plot_list) +
  plot_layout(nrow = 2, ncol = 2)
fig