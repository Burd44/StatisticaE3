parabola <- function(x) {
  return(-2*x^2 + 5*x - 2)
}

a <- 1/2
b <- 2

exact_area <- integrate(parabola, a, b)$value

n_points <- 10000

x_points <- runif(n_points, a, b)
y_points <- parabola(x_points)

dx <- (b - a) / n_points
estimated_area <- sum(y_points) * dx

relative_error <- abs(estimated_area - exact_area) / exact_area

cat("Exact area:", exact_area, "\n")
cat("Estimated area:", estimated_area, "\n")
cat("Relative error:", relative_error, "\n")