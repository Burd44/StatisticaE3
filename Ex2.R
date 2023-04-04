n=as.numeric(readline())
10
p=as.numeric(readline())
0.5
geo = function(p, n)
{
  pr=dgeom(1:n, p)
  for (i in 1:n)
  {
    print(pr[i])
  }
  barplot(pr)
}
