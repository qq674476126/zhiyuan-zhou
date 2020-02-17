# First Name:Zhiyuan
# Last Name:Zhou
# NetID:zz23
# Homework 2

###

## Exercise 1

## (a) read the data

wheel <- read.csv("C:/Users/67447/Downloads/roulette (1).csv")

## (b) function roulette()
roulette <- function(bet,amount=1){
  x = sample(0:36,1)
  amount <- -amount
  if(bet == "low" & x <=18 & x>0){amount <- -amount}
  if(bet == "high" & x >=19){amount <- -amount}
  if(bet == "red" & wheel[x+1,2] == "red"){amount <- -amount}
  if(bet == "black" & wheel[x+1,2] == "black"){amount <- -amount}
  if(bet == "odd" & x%%2 == 1){amount <- -amount}
  if(bet == "even" & x%%2 == 0){amount <- -amount}
  if(bet == "first" & x <= 12 & x > 0){amount <- -2*amount}
  if(bet == "second" & x >= 13 & x <= 24){amount <- -2*amount}
  if(bet == "third" & x >= 25){amount <- -2*amount}
  if(bet == x){amount <- -36*amount}
  else{amount=amount}
  return(cat("$",amount))
}


###

## Exercise 2
"a"
roulette2 <- function(bet,amount=1){
  x = sample(0:36,1)
  amount <- -amount
  if(bet == "low" & x <=18 & x>0){amount <- -amount}
  if(bet == "high" & x >=19){amount <- -amount}
  if(bet == "red" & wheel[x+1,2] == "red"){amount <- -amount}
  if(bet == "black" & wheel[x+1,2] == "black"){amount <- -amount}
  if(bet == "odd" & x%%2 == 1){amount <- -amount}
  if(bet == "even" & x%%2 == 0){amount <- -amount}
  if(bet == "first" & x <= 12 & x > 0){amount <- -2*amount}
  if(bet == "second" & x >= 13 & x <= 24){amount <- -2*amount}
  if(bet == "third" & x >= 25){amount <- -2*amount}
  if(bet == x){amount <- -36*amount}
  else{amount=amount}
  return(amount)
}

"b"
y = replicate(roulette2("red"),n=5000)
prob1 = length(y[y>=0])/5000
prob1

"c"
z = replicate(roulette2("first"),n=5000)
prob2 = length(z[z>=0])/5000
prob2

"d"
m = replicate(roulette2("odd",5),n=7500)
sum(m)/7500

###

## Exercise 3
"a"
hist(iris$Sepal.Length,xlab = "Sepal Length",ylab = "Density",main = "Histogram of Sepal Length")
"b"
plot(Sepal.Width~Sepal.Length,iris,main = "Iris Sepal Length vs. Sepal Width", xlab = "Sepal Length",ylab = "Sepal Width",col="pink")
"c"
plot(Sepal.Width~Sepal.Length,iris,main = "Iris Sepal Length vs. Sepal Width", xlab = "Sepal Length",ylab = "Sepal Width",col = c("green","red","blue")[Species])

