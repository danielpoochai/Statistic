data  = read.csv('/Users/daniel/Desktop/R HW5/Verizon.csv')

ALL = data$Time
ILEC = subset(data, select = Time, subset = Group == "ILEC",drop = T)
CLEC = subset(data, select = Time, subset = Group == "CLEC",drop = T)

u_ILEC = mean(ILEC)
u_CLEC = mean(CLEC)

u_pe = u_ILEC/u_CLEC 

#bootstrap
N = 10^4

nI = length(ILEC)
nC = length(CLEC)

bootMean = numeric(N)

for(i in 1:N){
  sampleI = sample(ILEC, nI, replace = TRUE)
  sampleC = sample(CLEC, nC, replace = TRUE)
  bootMean[i] = mean(sampleI)/mean(sampleC)
}

sd(bootMean)
mean(bootMean)

L <- quantile(bootMean,(0.025))
U <- quantile(bootMean,(0.975))

plot(density(bootMean))
abline(v = L,col = 'red', lty = 2)
abline(v = U,col = 'red', lty = 2)

#permutation test
N_p = 10^4 -1
result = numeric(N_p)
for(i in 1:N_p){
    index = sample(nI+nC, size = nI, replace = FALSE)
    result[i] = mean(ALL[index])/mean(ALL[-index])
}

plot(density(result))

observed = u_ILEC/u_CLEC
#abline(v = observed, col = "blue", lty = 5)

#compute P-value
p_value = (sum(result <= observed) +1)/ (N_p+1)
abline(v = p_value, col = "blue", lty = 5)


