
##### 05-1

favorite <- c('WINTER', 'SUMMER', 'SPRING', 'SUMMER', 'SUMMER',
              'FALL', 'FALL', 'SUMMER', 'SPRING', 'SPRING')

favorite
table(favorite)
table(favorite)/length(favorite)

##### 05-2

ds <- table(favorite)
ds

barplot(ds, main='favorite season')

##### 05-3

ds <- table(favorite)
ds

pie(ds, main='favorite season')

##### 05-4

favorite.color <- c(2, 3, 2, 1, 1, 2, 2, 1, 3, 2, 1, 3, 2, 1, 2)
ds <- table(favorite.color)
ds

barplot(ds, main='favorite color')

colors <- c('green', 'red', 'blue')
names(ds) <- colors
ds

barplot(ds, main='favorite color', col=colors)
pie(ds, main='favorite color', col=colors)

##### 05-5

weight <- c(60, 62, 64, 65, 68, 69)
weight.heavy <- c(weight, 120)
weight
weight.heavy

mean(weight)
mean(weight.heavy)

median(weight)
median(weight.heavy)

mean(weight, trim=0.2)
mean(weight.heavy,trim=0.2)

##### 05-6

mydata <- c(60, 62, 64, 65, 68, 69, 120)

quantile(mydata)
quantile(mydata, (0:10)/10)
summary(mydata)

##### 05-7

mydata <- c(60, 62, 64, 65, 68, 69, 120)

var(mydata)
sd(mydata)
range(mydata)
diff(range(mydata))

##### 05-8

dist <- cars[,2]
hist(dist,
     main="Histogram",
     xlab ="xlab",
     ylab="ylab",
     border="blue",
     col="green",
     las=2,
     breaks=5)

##### 05-9

dist <- cars[,2]
boxplot(dist, main="title")

##### 05-10

boxplot.stats(dist)

##### 05-11

boxplot(Petal.Length~Species, data=iris, main="title")

##### 한 화면에 그래프 여러 개 출력하기

par(mfrow=c(1,3))
barplot(table(mtcars$carb),
        main="Barplot of Carburetors",
        xlab="#of carburetors",
        ylab="frequency",
        col="blue")
barplot(table(mtcars$cyl),
        main="Barplot of Cylender",
        xlab="#of cylender",
        ylab="frequency",
        col="red")
barplot(table(mtcars$gear),
        main="Barplot of Grar",
        xlab="#of gears",
        ylab="frequency",
        col="green")
par(mfrow=c(1,1))
