library(MASS)
library(ggplot2)
library(plotly)

path = "/Users/daniel/Desktop/L2"
setwd(path)
data = read.csv("AssignmentDataFrame.csv")

#1.
for(i in colnames(data)){
  if(sum(is.na(data[,i]))!= 0){
    print(i)
    print(mean(data[,i],na.rm = TRUE))
    print(summary(data[,i]))
  }
}

#2.
data_order = data[order(data$BirthDay),]
data_order = data_order[order(data_order$BirthMonth),]
data_order = data_order[order(data_order$BirthYear),]
write.csv(data_order,"Q2.csv")

#3.
Doglover = data$DogLover
dog_num <- sum(Doglover[!is.na(Doglover)])
cat_num <- length(Doglover) - dog_num
result <- c(dog_num, cat_num)

barplot(result, names.arg = c('DogLovers', 'CatLovers'), main = 'Problem 3', col = c('brown', 'black'), border = TRUE)

#4.
Family_mems = data$NumFamilyMemb
Family_mems = Family_mems[!is.na(Family_mems)]
boxplot(Family_mems)
mean(Family_mems)
summary(Family_mems)

#5.
x = data$GuessNumber
x = x[!is.na(x)]
y = data$IQ
y = y[!is.na(y)]
plot(x, y, xlab = 'GuessNumber', ylab = 'IQ', main = 'Problem 5')













