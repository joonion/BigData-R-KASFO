
##### 06-1

wt <-mtcars$wt
mpg <- mtcars$mpg
plot(wt, mpg,
     main="중량-연비 그래프",
     xlab="중량",
     ylab="연비(MPG)",
     col="red",
     pch=19)

##### 06-2

vars <- c("mpg","disp","drat","wt")
target <- mtcars[,vars]
head(target)
pairs(target,main="Multi Plots")

##### 06-3

iris.2 <- iris[,3:4]
point <- as.numeric(iris$Species)
point
color <- c("red","green","blue")
plot(iris.2,
     main="Iris plot",
     pch=c(point),
     col=color[point])

##### 06-4

beers = c(5, 2, 9, 8, 3, 7, 3, 5, 3, 5)
bal <- c(0.1, 0.03, 0.19, 0.12, 0.04, 0.0095, 0.07, 0.06, 0.02, 0.05)
tbl <- data.frame(beers,bal)
tbl

plot(bal~beers, data=tbl)
res <- lm(bal~beers, data=tbl)
abline(res)
cor(beers,bal)

##### 06-5

cor(iris[,1:4])

##### 06-6

month = 1:12
late  = c(5,8,7,9,4,6,12,13,8,6,6,4)
plot(month,
     late,
     main="지각생 통계계",
     type= "l",
     lty=1,
     lwd=1,
     xlab="Month",
     ylab="Late cnt"
)

##### 06-7

month = 1:12
late1  = c(5,8,7,9,4,6,12,13,8,6,6,4)
late2  = c(4,6,5,8,7,8,10,11,6,5,7,3)

plot(month,
     late1,
     main="Late Students",
     type= "b",
     lty=1,
     col="red",
     xlab="Month ",
     ylab="Late cnt",
     ylim=c(1, 15)
)

lines(month,
      late2,
      type = "b",
      col = "blue")

##### 06-8

library(mlbench)
data("BostonHousing")
myds <- BostonHousing[,c("crim","rm","dis","tax","medv")]

grp <- c()
for (i in 1:nrow(myds)) {
  if (myds$medv[i] >= 25.0) {
    grp[i] <- "H"
  } else if (myds$medv[i] <= 17.0) {
    grp[i] <- "L"
  } else {
    grp[i] <- "M"
  }
}

grp <- factor(grp)
grp <- factor(grp, levels=c("H","M","L"))

myds <- data.frame(myds, grp)

str(myds)
head(myds)
table(myds$grp)

par(mfrow=c(2,3))
for(i in 1:5) {
  hist(myds[,i], main=colnames(myds)[i], col="yellow")
}
par(mfrow=c(1,1))

par(mfrow=c(2,3))
for(i in 1:5) {
  boxplot(myds[,i], main=colnames(myds)[i])
}
par(mfrow=c(1,1))

boxplot(myds$crim~myds$grp, main="1인당 범죄율")
boxplot(myds$rm~myds$grp, main="방의 수")
boxplot(myds$dis~myds$grp, main="직업센터까지의 거리")
boxplot(myds$tax~myds$grp, main="제산세")

pairs(myds[,-6])
point <- as.integer(myds$grp)
color <- c("red","green","blue")
pairs(myds[,-6], pch=point, col=color[point])

cor(myds[,-6])
