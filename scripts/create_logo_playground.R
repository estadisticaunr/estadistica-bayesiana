formula <- "f(\\phi | y) \\propto f(\\phi) \\times L(\\phi | y)"

alpha <- 4
beta <- 4
n <- 14
x <- 11

alpha_posterior <- alpha + x
beta_posterior <- beta + n - x

grid <- seq(0, 1, length.out = 500)
pdf_prior <- dbeta(grid, alpha, beta)
pdf_posterior <- dbeta(grid, alpha_posterior, beta_posterior)


likelihood_f <- function(value) {
    dbinom(x = x, size = n, prob = value)
}
likelihood <- likelihood_f(grid)
likelihood_area <- integrate(likelihood_f, lower = 0, upper = 1)[[1]]
likelihood_scaled <- likelihood / likelihood_area


# f5f0e1
colors <- c("#1e3d59", "#ff6e40", "#ffc13b")

fill_with_area <- FALSE

par(mar = c(0, 0, 0, 0))
plot(
    grid, pdf_prior, type = "l", xlim=c(0, 1), ylim=c(0, 4.5), 
    col=colors[3], lwd = 4, 
    bty = "n", yaxt = "n", xaxt = "n", ylab = "", xlab = ""
)


if (fill_with_area) {
    polygon(grid, pdf_prior, col = adjustcolor(colors[3], alpha.f = 0.4), border = NA)
    polygon(grid, likelihood_scaled, col = adjustcolor(colors[1], alpha.f = 0.4), border = NA)
    polygon(grid, pdf_posterior, col = adjustcolor(colors[2], alpha.f = 0.4), border = NA)
} else {
    polygon(grid, pdf_prior, col=colors[3], density = 10, border = NA, lwd = 3)
    polygon(grid, likelihood_scaled, col=colors[1], density = 10, border = NA, lwd = 3)
    polygon(grid, pdf_posterior, col=colors[2], density = 10, border = NA, lwd = 3)

    # Opcional?
    polygon(grid, pdf_prior, col = adjustcolor(colors[3], alpha.f = 0.1), border = NA)
    polygon(grid, likelihood_scaled, col = adjustcolor(colors[1], alpha.f = 0.1), border = NA)
    polygon(grid, pdf_posterior, col = adjustcolor(colors[2], alpha.f = 0.1), border = NA)

}

lines(grid, likelihood_scaled, col=colors[1], lwd = 4)
lines(grid, pdf_posterior, col=colors[2], lwd = 4)


dev.off()

# colores
# https://99designs.com/blog/creative-inspiration/color-combinations/\