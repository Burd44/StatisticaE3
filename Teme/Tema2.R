#Exercitiul 1
n=c(5000, 10000, 100000, 500000)
p=c(0.2, 0.6, 0.6, 0.8)

LLN_Geometric = function(p, n) 
{
  for(i in 1:4)
  {
    for(j in 1:4)
    {
      print(mean(rgeom(n[i], p[j])))
    }
  }
}

LLN_Geometric(p, n)

#Exercitiul 2
N=c(5000, 10000, 20000)
z=c(-1.5, 0, 1.5)
n=50

CLT_Student = function(r, n, N, z) 
{
  for(i in 1:3)
  {
    for(k in 1:3)
    {
      expectation = 0
      st_dev = sqrt(r/(r-2))
      upper_bound = z[k] * st_dev/sqrt(n) + expectation
      sum = 0
      for(j in 1:N[i]) 
      {
        x_n = mean(dt(n, r))
        if(x_n <= upper_bound) 
        {
          sum = sum + 1
        }
      }
      print(c(sum/N[i], pnorm(z[k])))
    }
  }
}

CLT_Student(10, n, N, z)

#Exercitiul 3
B = function(n, p, h, k) 
{
  expectation = n*p;
  variance = n*p*(1 - p);
  standard_deviation = sqrt(variance);
  q1 = (h + 0.5 - expectation)/standard_deviation;
  q2 = (k - 0.5 - expectation)/standard_deviation;
  return(1 - pnorm(q1 - q2));
}

B(50, 0.3, 20, 35)