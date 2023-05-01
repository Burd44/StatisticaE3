#Exercitiul 1
Ex1 = function(l, p, n, k)
{
  poss=dpois(k:n, l)
  geo=dgeom(k:n, p)
  b=dbinom(k:n, (n-k), p)
  v=vector()
  j=1
  for(i in 1:(n-k))
  {
    v[j]=poss[i]
    j=j+1
  }
  for(i in 1:(n-k))
  {
    v[j]=geo[i]
    j=j+1
  }
  for(i in 1:(n-k))
  {
    v[j]=b[i]
    j=j+1
  }
  barplot(v)
}

Ex1(5, 0.2, 20, 7)

#Exercitiul 2 punctul a
Ex2a = function(x)
{
  v = read.csv(x, header = T, sep = ",", colClasses=c('numeric','numeric'))
  P = v[['P']]
  S = v[['S']]
  cat("Mean of P: ")
  cat(mean(P))
  cat('\n')
  cat("Median of P: ")
  cat(median(P))
  cat('\n')
  cat("Standrad Deviation of P: ")
  cat(sd(P))
  cat('\n')
  cat("Quartiles of P: ")
  cat(as.vector(quantile(P)))
  cat('\n')
  cat('\n')
  cat("Mean of S: ")
  cat(mean(S))
  cat('\n')
  cat("Median of S: ")
  cat(median(S))
  cat('\n')
  cat("Standrad Deviation of S: ")
  cat(sd(S))
  cat('\n')
  cat("Quartiles of S: ")
  cat(as.vector(quantile(S)))
}

Ex2a("note.csv")

#Exercitiul 2 punctul b
Ex2b = function(x, y)
{
  v = read.csv(x, header = T, sep = ",", colClasses=c('numeric','numeric'))
  b=vector()
  j=1
  if ( y =='P')
  {
    a = v[['P']]
  }
  if (y=='S')
  {
    a = v[['S']]
  }
  s=sd(a)
  m=mean(a)
  for(i in 1:length(a))
  {
    if(a[i] >= m-2*s & a[i] <= m+2*s)
    {
      b[j] = a[i]
      j=j+1
    }
  }
  if ( y =='P')
  {
    write.table(b, "P.txt", sep="")
  }
  if (y=='S')
  {
    write.table(b, "S.txt", sep="")
  }
  print(b)
}

Ex2b("note.csv", 'S')
Ex2b("note.csv", 'P')

#Exercitiul 2 punctul c
Ex2c = function(x)
{
  interval = seq(1, 10, 1)
  v = read.delim(x, header=F, sep="", dec = ".")
  v=v[['V2']]
  hist(v,  breaks = interval, include.lowest = T, right = T, freq = T)
}

Ex2c("P.txt")
Ex2c("S.txt")