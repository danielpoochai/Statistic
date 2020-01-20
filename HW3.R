#2.1
#1
p = 1 - pnorm(168,153,11)
print(p)

#2
set.seed(1234)

students1 = rnorm(1000, 153, 11)
which(students1>168)
prob1 = length(which(students1>168))/length(students1)
print(prob1)

students2 = rnorm(10000, 153, 11)
which(students2>168)
prob2 = length(which(students2>168))/length(students2)
print(prob2)

students3 = rnorm(100000, 153, 11)
which(students3>168)
prob3 = length(which(students3>168))/length(students3)
print(prob3)

#2.2
#1
path = '/Users/daniel/Desktop/台大課程/大四上/統計學暨實習/助教課/L3'
setwd(path)
data1 = read.csv('AAPL.csv')
data2 = read.csv('INTC.csv')
stock_price = cbind(data1$Open, data2$Open)
colnames(stock_price) = c('APPLE', 'INTEL')
stock = data.frame(stock_price)

r1 = diff(log(stock$APPLE),1)
hist(r1)
r2 = diff(log(stock$INTEL),1)
hist(r2)

mean(r1)*251
mean(r2)*251
sd(r1)*251
sd(r2)*251
cov(r1,r2)

returnP = function(omega_r1){
  retP = omega_r1 * mean(r1) + (1-omega_r1) * mean(r2)
  return(retP)
}

sigmaP = function(omega_r1){
  varP = omega_r1^2*var(r1) + (1 - omega_r1)^2*var(r2) + 2*omega_r1*(1 - omega_r1)*cov(r1,r2)
  return(sqrt(varP))
}

omega = seq(0,1,0.01)

#Efficient Frontier
plot(sigmaP(omega), returnP(omega), type = 'l', main="Efficient Frontier of P consisting of APPLE & Intel",
     ylab = "Expected Return (r)", xlab = "risk(sigma)")

#min 
min_omega = omega[which.min(sigmaP(omega))]
print(min_omega)



