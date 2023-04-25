birth = read.csv("life_expect.csv", header = T, sep = ',')
nr = 7
hist(birth$female, breaks = nr)
hist(birth$male, breaks = nr)