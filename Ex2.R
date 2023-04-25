n=as.numeric(readline())
10
l=as.numeric(readline())
5
poss = function(l, n)
{
  p=dpois(1:n, l)
  for (i in 1:n)
  {
    print(p[i])
  }
  barplot(p)
}
