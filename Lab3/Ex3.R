N=c(5000, 10000, 20000)
z=c(-1.5, 0, 1.5)
n=50

CLT_Gamma = function(alpha, lambda, n, N, z) 
{
  for(i in 1:3)
  {
    for(k in 1:3)
    {
      expectation = alpha/lambda
      st_dev = sqrt(alpha/(lambda*lambda))
      upper_bound = z[k] * st_dev/sqrt(n) + expectation
      sum = 0
      for(j in 1:N[i]) 
      {
        x_n = mean(rgamma(n, alpha, lambda))
        if(x_n <= upper_bound) 
        {
          sum = sum + 1
        }
      }
      print(c(sum/N[i], pnorm(z[k])))
    }
  }
}

CLT_Gamma(10, 10, n, N, z)