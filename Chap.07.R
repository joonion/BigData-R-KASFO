
##### 07-1

z <- c(1,2,3,NA,5,NA,8)
sum(z)
is.na(z)
sum(is.na(z))
sum(z, na.rm=TRUE)

##### 07-2

z1 <- c(1,2,3,NA,5,NA,8)
z2 <- c(5,8,1,NA,3,NA,7)
z1[is.na(z1)] <- 0
z1

z3 <- as.vector(na.omit(z2))
z3

##### 07-3

x <- iris
x[1,2]<- NA; x[1,3]<- NA
x[2,3]<- NA; x[3,4]<- NA

head(x)

##### 07-4

for (i in 1:ncol(x)) {
  this.na <- is.na(x[,i])
  cat(colnames(x)[i], "\t", sum(this.na), "\n")
}

col_na <- function(y) {
  return(sum(is.na(y)))
}

na_count <-apply(x, 2, FUN=col_na)
na_count

##### 07-5

rowSums(is.na(x))
sum(rowSums(is.na(x))>0)
sum(is.na(x))

##### 07-6

head(x)

x[!complete.cases(x),]
y <- x[complete.cases(x),]
head(y)

##### 07-7

st <- data.frame(state.x77)

boxplot(st$Income)
boxplot.stats(st$Income)$out

##### 07-8

out.val <- boxplot.stats(st$Income)$out
st$Income[st$Income %in% out.val] <- NA
head(st)

newdata <- st[complete.cases(st),]
head(newdata)

##### 07-9

v1 <- c(1,7,6,8,4,2,3)
order(v1)

v1 <- sort(v1)
v1

v2 <- sort(v1, decreasing=T)
v2

##### 07-10

head(iris)
order(iris$Sepal.Length)

iris[order(iris$Sepal.Length),]
iris[order(iris$Sepal.Length, decreasing=T),]

iris.new <- iris[order(iris$Sepal.Length),]
head(iris.new)

iris[order(iris$Species, decreasing=T, iris$Petal.Length),]

##### 07-11

sp <- split(iris, iris$Species)
sp
summary(sp)
sp$setosa

##### 07-12

subset(iris, Species == "setosa")

subset(iris, Sepal.Length > 7.5)

subset(iris, Sepal.Length > 5.1 & Sepal.Width > 3.9)

subset(iris, Sepal.Length > 7.6,
       select=c(Petal.Length,Petal.Width))

##### 07-13

x <- 1:100

y <- sample(x, size=10, replace = FALSE)
y

##### 07-14

idx <- sample(1:nrow(iris), size=50, replace = FALSE)

iris.50 <- iris[idx,]
dim(iris.50)
head(iris.50)

##### 07-15

sample(1:20, size=5)

sample(1:20, size=5)

sample(1:20, size=5)

set.seed(100)
sample(1:20, size=5)

set.seed(100)
sample(1:20, size=5)

set.seed(100)
sample(1:20, size=5)

##### 07-16

combn(1:5,3)

x = c("red","green","blue","black","white")
com <- combn(x,2)
com

for(i in 1:ncol(com)) {
  cat(com[,i], "\n")
}

##### 07-17

agg <- aggregate(iris[,-5], by=list(iris$Species), 
                 FUN=mean)
agg

##### 07-18

agg <- aggregate(iris[,-5], by=list(Species=iris$Species), 
                 FUN=sd)

agg

##### 07-19

head(mtcars)

agg <- aggregate(mtcars, by=list(cyl=mtcars$cyl, vs=mtcars$vs),
                 FUN=max)
agg

##### 07-20

x <- data.frame(name=c("a","b","c"), math=c(90,80,40))
y <- data.frame(name=c("a","b","d"), korean=c(75,60,90))
x
y

##### 07-21

z <- merge(x, y, by=c("name"))
z

##### 07-22

merge(x, y, all.x=T)
merge(x, y, all.y=T)
merge(x, y, all=T)

##### 07-23

x <- data.frame(name=c("a","b","c"), math=c(90,80,40))
y <- data.frame(sname=c("a","b","d"), korean=c(75,60,90))
x
y

merge(x, y, by.x=c("name"), by.y=c("sname"))
