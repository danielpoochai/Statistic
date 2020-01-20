A_tmp <- sample(c("Head","Tail"), size = 3, replace = T, prob = c(0.65,0.35))
print(A_tmp)
A <- sum(A_tmp[] == "Head")
print(A)
#pmf binomial p=0.65
x <- seq(from = 0, to = 3, by = 1)
pmf <- dbinom(x, size = 3, prob = 0.65)
plot(x, pmf, 'p')

#cdf 
cdf <- pbinom(x, size = 3, prob = 0.65)
plot(x, cdf, 's')