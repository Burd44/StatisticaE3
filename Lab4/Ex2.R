MC_improved_integration = function(N) 
{
  sum = 0;
  for(i in 1:N) 
  {
    u = rexp(1, 3);
    sum = sum + exp(-2*u*2*u);
  }
  return(sum/N);
}

MC_imprvd_integr_average= function(k, N) 
  {
  estimates = 0;
  for(i in 1:k)
    estimates[i] = MC_improved_integration(N);
  print(mean(estimates));
  print(sd(estimates));
}

MC_imprvd_integr_average(30, 50000)