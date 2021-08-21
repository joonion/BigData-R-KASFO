
##### 03-1

z <- matrix(1:20, nrow=4, ncol=5)
z

##### 03-2

z2 <- matrix(1:20, nrow=4, ncol=5, byrow=T)
z2

##### 03-3

x <- 1:4
y <- 5:8
z <- matrix(1:20, nrow=4, ncol=5)

m1 <- cbind(x,y)
m1
m2 <- rbind(x,y)
m2
m3 <- rbind(m2,x)
m3
m4 <- cbind(z,x)
m4

##### 03-4

z <- matrix(1:20, nrow=4, ncol=5)
z

z[2, 3]
z[1, 4]
z[2,]
z[,4]

##### 03-5

z <- matrix(1:20, nrow=4, ncol=5)
z

z[2, 1:3]
z[1, c(1,2,4)]
z[1:2,]
z[,c(1,4)]

##### 03-6

score <- matrix(c(90,85,69,78,
                  85,96,49,95,
                  90,80,70,60),
                  nrow=4, ncol=3)
score

rownames(score) <- c('John', 'Tom', 'Mark', 'Jane')
colnames(score) <- c('English', 'Math', 'Science')
score

##### 03-7

score['John', 'Math']
score['Tom', c('Math','Science')]
score['Mark',]
score[,'English']

rownames(score)
colnames(score)
colnames(score)[2]

##### 03-8

city <- c("Seoul", "Tokyo", "Washington")
rank <- c(1, 3, 2)
city.info <- data.frame(city, rank)
city.info

##### 03-9

iris[,c(1:2)]
iris[,c(1,3,5)]
iris[,c("Sepal.Length","Species")]
iris[1:5,]
iris[1:5,c(1,3)]

##### 03-10

dim(iris)
nrow(iris)
ncol(iris)
colnames(iris)
head(iris)
tail(iris)

##### 03-11

str(iris)
iris[,5]
unique(iris[,5])
table(iris[,"Species"])

##### 03-12

colSums(iris[,-5])
colMeans(iris[,-5])
rowSums(iris[,-5])
rowMeans(iris[,-5])

##### 03-13

z <- matrix(1:20, nrow=4, ncol=5)
z
t(z)

##### 03-14

IR.1 <- subset(iris, Species=="setosa")
IR.1

IR.2 <- subset(iris, Sepal.Length>5.0 & Sepal.Width>4.0)
IR.2
IR.2[, c(2,4)]

##### 03-15

a <- matrix(1:20, 4, 5)
a
b <- matrix(21:40, 4, 5)
b

2 * a
b - 5
2*a + 3*b

a + b
b - a
b / a
a * b

a <- a * 3
b <- b - 5

##### 03-16

class(iris)
class(state.x77)
is.matrix(iris)
is.data.frame(iris)
is.matrix(state.x77)
is.data.frame(state.x77)

##### 03-17

st <- data.frame(state.x77)
head(st)
class(st)

iris.m <- as.matrix(iris[,1:4])
head(iris.m)
class(iris.m)

##### 03-18

iris[,"Species"]
iris[,5]
iris["Species"]
iris[5]
iris$Species

##### 03-19

getwd()
setwd(".")
air <- read.csv("./datasets/airquality.csv", header = T)
head(air)

##### 03-20

my.iris <- subset(iris, Species='Setosa')
write.csv(my.iris, "./datasets/my_iris.csv", row.names = F)

##### 엑셀 파일 읽기
install.packages("readxl")
library(readxl)

air2 <- read_excel("./datasets/airquality.xlsx", sheet = 2)
head(air2)

