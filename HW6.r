dice = function(n){
  X = sample(1:6, size = n, replace = T)
  return(mean(X))
}

Xbar10 = replicate(10000, dice(10))
Xbar100 = replicate(10000, dice(100))
Xbar1000 = replicate(10000, dice(1000))

plot(density(Xbar10))
plot(density(Xbar100))
plot(density(Xbar1000))


