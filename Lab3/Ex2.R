n=c(1000, 10000, 100000, 1000000)
r=c(2, 3, 4, 5)

LLN_Student = function(r, n) 
{
  for(i in 1:4)
  {
    for(j in 1:4)
    {
      print(mean(dt(n[i], r[j])))
    }
  }
}

LLN_Student(r, n)