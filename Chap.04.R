
##### 04-1

job.type <- 'A'
if (job.type == 'B') {
  bonus <- 200
} else {
  bonus <- 100
}

print(bonus)

##### 04-2

job.type <- 'B'
bonus <- 100

if (job.type == 'A') {
  bonus <- 200
}

print(bonus)

##### 04-3

score <- 85

if (score > 90) {
  grade <- 'A'
} else if (score > 80) {
  grade <- 'B'
} else if (score > 70) {
  grade <- 'C'
} else if (score > 60) {
  grade <- 'D'
} else {
  grade <- 'F'
}

print(grade)

##### 04-4

a <- 10
b <- 20

if (a > 5 & b > 5) {
  print (a+b)
}

if (a > 5 | b > 30) {
  print (a*b)
}

##### 04-5

a <- 10
b <- 20

if (a > b) {
  c <- a
} else {
  c <- b
}

print(c)

a <- 10
b <- 20
c <- ifelse(a > b, a, b)

print(c)

##### 04-6

for(i in 1:5) {
  print('*')
}

##### 04-7

for(i in 6:10) {
  print(i)
}

##### 04-8

for(i in 1:9) {
  cat('2 *', i,'=', 2*i,'\n')
}

##### 04-9

for(i in 1:20) {
  if(i %% 2 == 0) {
    print(i)
  }
}

##### 04-10

sum <- 0
for(i in 1:100) {
  sum <- sum + i
}
print(sum)

##### 04-11

norow <- nrow(iris)
mylabel <- c( )
for(i in 1:norow) {
  if (iris$Petal.Length[i] <= 1.6) {
    mylabel[i] <- 'L'
  } else if (iris$Petal.Length[i] >= 5.1) {
    mylabel[i] <- 'H'
  } else {
    mylabel[i] <- 'M'
  }
}

print(mylabel)

newds <- data.frame(iris$Petal.Length, mylabel)
head(newds)

##### 04-12

sum <- 0
i <- 1
while(i <=100) {
  sum <- sum + i
  i <- i + 1
}

print(sum)

##### 04-13

sum <- 0
for(i in 1:10) {
  sum <- sum + i
  if (i>=5) break
}
sum

##### 04-14

sum <- 0
for(i in 1:10) {
  if (i%%2 == 0) next
  sum <- sum + i
}
sum

##### 04-15

apply(iris[,1:4], 1, mean)
apply(iris[,1:4], 2, mean)

##### 04-16

mymax <- function(x,y) {
  num.max <- x
  if (y > x) {
    num.max <- y
  }
  return(num.max)
}

##### 04-17

mymax(10,15)
a <- mymax(20,15)
b <- mymax(31,45)
print(a+b)

##### 04-18

mydiv <- function(x,y=2) {
  result <- x/y
  return(result)
}

mydiv(x=10,y=3)
mydiv(10,3)
mydiv(10)

##### 04-19

myfunc <- function(x,y) {
  val.sum <- x+y
  val.mul <- x*y
  return(list(sum=val.sum, mul=val.mul))
}

result <- myfunc(5,8)

s <- result$sum
m <- result$mul

cat('5+8=', s, '\n')
cat('5*8=', m, '\n')

##### 04-20

source("myfunc.R")

a <- mydiv(20,4)
b <- mydiv(30,4)
a + b

mydiv(mydiv(20,2),5)

##### 04-21

score <- c(76, 84, 69, 50, 95, 60, 82, 71, 88, 84)

which(score==69)
which(score>=85)
max(score)
which.max(score)
min(score)
which.max(score)

##### 04-22

score <- c(76, 84, 69, 50, 95, 60, 82, 71, 88, 84)

idx <- which(score<=60)
score[idx] <- 61
score

idx <- which(score>=80)
score.high <- score[idx]
score.high

##### 04-23

idx <- which(iris$Petal.Length>5.0)
idx

iris.big <- iris[idx,]
iris.big

##### 04-24

idx <- which(iris[,1:4]>5.0, arr.ind =TRUE)
idx

