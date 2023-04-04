p=as.numeric(readline())
10
l=as.numeric(readline())
0.5
geo = function(l, p)
{
  pr=dgeom(1:p, l)
  for (i in 1:p)
  {
    print(pr[i])
  }
  barplot(pr)
}