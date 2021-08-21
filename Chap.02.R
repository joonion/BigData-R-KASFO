
##### 02-1

2 + 3
(3 + 6) * 8
2^3

##### 02-2

7 + 4
# 2^3


##### 02-3

log(10) + 5
sqrt(25)
max(5, 3, 2)

##### 02-4

a <- 10
b <- 20
c <- a + b
print(c)

##### 02-5

a <- 125
a
print(a)

##### 02-6

a <- 10
b <- 20
a + b
a <- "A"
a + b

##### 02-7

x <- c(1, 2, 3)
y <- c("a", "b", "c")
z <- c(TRUE, TRUE, FALSE, TRUE)
x
y
z

##### 02-8

w <- c(1, 2, 3, "a", "b", "c")
w

##### 02-9

v1 <- 50:90
v1

v2 <- c(1, 2, 5, 50:90)
v2

##### 02-10

v3 <- seq(1, 101, 3)
v3

v4 <- seq(0.1, 1.0, 0.1)
v4

##### 02-11

v5 <- rep(1, times=5)
v5

v6 <- rep(1:5, times=3)
v6

v7 <- rep(c(1,5,9), times=3)
v7

##### 02-12

score <- c(90, 85, 70)
score

names(score)
names(score) <- c("John", "Tom", "Jane")
names(score)
score

##### 02-13

d <- c(1, 4, 3, 7, 8)
d[1]
d[2]
d[3]
d[4]
d[5]
d[6]

##### 02-14

d <- c(1, 4, 3, 7, 8)

d[c(1, 3, 5)]
d[1:3]
d[seq(1, 5, 2)]
d[-2]
d[-c(3:5)]

##### 02-15

GNP <- c(2090, 2450, 960)
GNP

names(GNP) <- c("Korea", "Japan", "Nepal")
GNP

GNP[1]
GNP["Korea"]
GNP[c("Korea", "Nepal")]

##### 02-16

v1 <- c(1, 5, 7, 8, 9)
v1

v1[2] <- 3
v1

v1[c(1,5)] <- c(10,20)
v1

##### 02-17

d <- c(1, 4, 3, 7, 8)

2 * d
d - 5
3 * d + 4

##### 02-18

x <- c(1, 2, 3)
y <- c(4, 5, 6)

x + y
x * y
z <- x + y
z

##### 02-19

d <- c(1,2,3,4,5,6,7,8,9,10)

sum(d)
sum(2*d)
length(d)
mean(d[1:5])
max(d)
min(d)
sort(d)
sort(d, decreasing = FALSE)
sort(d, decreasing = TRUE)

v1 <- median(d)
v1

v2 <- sum(d)/length(d)
v2

##### 02-20

d <- c(1,2,3,4,5,6,7,8,9)

d >= 5
d[d > 5]
sum(d > 5)
sum(d[d > 5])
d == 5

condi <- d > 5 & d < 8
d[condi]

##### 02-21

ds <- c(90, 85, 70, 84)

my.info <- list(name='Tom', age=60, status=TRUE, score=ds)

my.info
my.info[[1]]
my.info$name
my.info[[4]]

##### 02-22

bt <- c('A', 'B', 'B', 'O', 'AB', 'A')
bt.new <- factor(bt)
bt
bt.new
bt[5]
bt.new[5]
levels(bt.new)
as.integer(bt.new)
bt.new[7] <- 'B'
bt.new[8] <- 'C'
bt.new
