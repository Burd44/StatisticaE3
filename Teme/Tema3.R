#Exercitiul 1
N=c(10000, 20000, 50000);
a=c(2, 4, 10);

Volume = function(N, a) 
{
  errors=vector()
  for(k in 1:3)
  {
    actual_value = pi*(a[i]^2)/2
    for(o in 1:3)
    {
      N_C = 0;
      for(i in 1:N[k]) 
      {
        x = runif(1, -sqrt(a[o]), sqrt(a[o]));
        y = runif(1, -sqrt(a[o]), sqrt(a[o]));
        z = runif(1, 0, a[o]);
        if(x3 >= x^2 + y^2 & z <= a[o])
          N_C = N_C + 1;
      }
      estimate_value = (4*a^3*N_C)/N[j]
      relative_error = abs(estimate_value - actual_value)/actual_value
      errors[j] = relative_error
    }
  }
}

Volume(N, a)

#Exercitiul 2
patrulater_area = function(N) 
{
  N_C = 0;
  for(i in 1:N) 
  {
    x = runif(1, 0, 3);
    y = runif(1, -4, 2);
    if(x*x + y*y <= 1)
      N_C = N_C + 1;
  }
  return(4*N_C/N);
}

patrulater_area(20000) #a=0, b=3, c=4, d=2

#Exercitiul 3 a
MC_improved_integration = function() 
{
  sum = 0;
  x = runif(1, -1, 1);
  sum = sum + ((x + 1)/sqrt(4-x*x));
  print(pi/3);
  return(sum);
}

MC_improved_integration()

#Exercitiul 3 b
MC_improved_integration = function() 
{
  sum = 0;
  x = runif(1, -10, 0);
  sum = sum + (1/(x*x+4));
  print(pi/4);
  return(sum);
}

MC_improved_integration()

#Exercitiul 3 c
MC_improved_integration = function() 
{
  sum = 0;
  x = runif(1, -10, 0);
  sum = sum + x*exp(x);
  print(-1);
  return(sum);
}

MC_improved_integration()

#Exercitiul 4 a
q=0.1
E=1/q
print(E)

#Exercitiul 4 b
n = 500
p = 0.5
zile = 40
prob <- pbinom(50000, zile, p)
print(prob)
