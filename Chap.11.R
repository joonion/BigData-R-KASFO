
##### 11-1

head(cars)
plot(dist~speed, data=cars)

model <- lm(dist~speed, cars)
model

abline(model)

coef(model)[1]
coef(model)[2]

##### 11-2

b <- coef(model)[1]
W <- coef(model)[2]

speed <- 30
dist <- W*speed + b
dist

speed <- 35
dist <- W*speed + b
dist

speed <- 40
dist <- W*speed + b
dist

##### 11-3

speed <- cars[,1]
pred <- W * speed + b
pred

compare <- data.frame(pred, cars[,2], pred-cars[,2])

colnames(compare) <- c('예상','실제','오차')
head(compare)

##### 11-4

library(car)
head(Prestige)

newdata <- Prestige[,c(1:4)]
plot(newdata, pch=16, col="blue",
     main="Matrix Scatterplot")

mod1 <- lm(income ~ education + prestige + women, 
           data=newdata)
summary(mod1)

##### 11-5

library(MASS)
newdata2 <- Prestige[,c(1:5)]
head(newdata2)

mod2 <- lm(income ~ education + prestige + women + census, 
           data= newdata2)

mod3 <- stepAIC(mod2)
mod3

summary(mod3)

##### 11-6

iris.new <- iris

iris.new$Species <- as.integer(iris.new$Species)
head(iris.new)

mod.iris <- glm(Species ~., data=iris.new)
summary(mod.iris)

##### 11-7

unknown <- data.frame(rbind(c(5.1, 3.5, 1.4, 0.2)))
names(unknown) <- names(iris)[1:4]
unknown

pred <- predict(mod.iris, unknown)
pred
round(pred,0)

pred <- round(pred,0)
pred

levels(iris$Species)
levels(iris$Species)[pred]

##### 11-8

test <- iris[,1:4]
pred <- predict(mod.iris, test)
pred <- round(pred,0)

pred
answer <- as.integer(iris$Species)
pred == answer
acc <- mean(pred == answer)
acc
