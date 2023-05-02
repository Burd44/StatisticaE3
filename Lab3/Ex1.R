density_normal = function(niu, delta, n, a) 
{
  x = seq(-a, a, n)
  y = dnorm(x, niu, delta)
  plot(x, y, type="l")
}

density_normal(0, 0.5, 0.1, 5)