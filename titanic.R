# Titanic: Machine Learning from Disaster

# for (reproducible) random number generation 
set.seed(0)

getwd()
# read train data
train <- read.csv('./datasets/titanic_train.csv')
train
head(train)
str(train)
summary(train)

# read test data

test <- read.csv('./datasets/titanic_test.csv')
test
head(test)
str(test)
summary(test)

# make a full dataset

test$Survived <- NA
full <- rbind(train, test)
full
head(full)
str(full)
summary(full)
tail(full)
#View(full)

# Rename Sex to Gender

renamed <- names(full)
renamed[5] <- 'Gender'
renamed
names(full) <- renamed
names(full)
str(full)

# Write intermediate result

write.csv(full, './datasets/titanic.merged.csv', row.names = F)

# Are there missing values?

attach(full)
any(is.na(PassengerId))
any(is.na(Survived))
any(is.na(Pclass))
any(is.na(Name))
any(is.na(Gender))
any(is.na(Age))
any(is.na(SibSp))
any(is.na(Parch))
any(is.na(Ticket))
any(is.na(Fare))
any(is.na(Cabin))
any(is.na(Embarked))
detach(full)

# How many missing values?

which(is.na(full$Age))
length(which(is.na(full$Age)))
which(is.na(full$Fare))

# Handling missing values 1: Fare

#install.packages('ggplot2')
library(ggplot2)

full[full$PassengerId == 1044, ]
df1 <- full[full$Embarked == 'S' & full$Pclass == 3, ]
str(df1)
ggplot(df1,
       aes(x = Fare)) + 
    geom_density(
        colour = 'blue', 
        fill = 'green', 
        alpha = 0.4)
summary(df1)

ggplot(df1,
       aes(x = Fare)) + 
    geom_density(
        colour = 'blue', 
        fill = 'green', 
        alpha = 0.4
    ) +
    geom_vline(
        aes(xintercept = median(Fare, na.rm = T)),
        colour = 'red',
        linetype = 'dashed', 
        lwd = 1
    ) +
    geom_vline(
        aes(xintercept = mean(Fare, na.rm = T)),
        colour = 'blue',
        linetype = 'dashed', 
        lwd = 1
    )

# Now, fill the fare with median value

full$Fare[1044] <- median(df1$Fare, na.rm = T)
full$Fare[1044]
which(is.na(full$Fare))

# Handing missing value 2: Age

n <- length(which(is.na(full$Age)))
n
existing_ages <- na.omit(full$Age)
existing_ages
rand_ages <- sample(existing_ages, n)
rand_ages

full[is.na(full$Age), ]$Age <- rand_ages
which(is.na(full$Age))

# Is it fair?

par(mfrow = c(1, 2))
hist(existing_ages, 
     main = 'Before', 
     col = 'green')
hist(full$Age, 
     main = 'After', 
     col = 'blue')
par(mfrow = c(1, 1))

# Handling Missing Values 3: Embarked

full[full$Embarked == '', ]
table(full$Embarked)
full[full$Embarked == '', ]$Embarked <- 'S'
table(full$Embarked)

# Handling missing values 4: Cabin

full$Cabin
cabin <- full$Cabin
n <- length(cabin)
numcabin <- vector(length = n)
for (i in 1:n) {
    if (cabin[i] != '') {
        cab <- unlist(strsplit(cabin[i], ' '))
        numcabin[i] <- length(cab)
    }
}

table(numcabin)
full$NumCabin <- numcabin

# Other preprocessing 1: Name
full$Name
name <- full$Name
n <- length(name)
title <- vector(length = n)
for (i in 1:n) {
    lastname = unlist(strsplit(name[i], ', '))[2]
    title[i] = unlist(strsplit(lastname, '. '))[1]
}

table(title)
title[title != 'Master' &
          title != 'Miss' &
          title != 'Mr' &
          title != 'Mrs'] <- 'Other'
table(title)
full$Title <- title

# Other preprocessing 2: Sibsp & Parch
full$NumFamily <- full$SibSp + full$Parch

processed <- full[c(1, 2, 3, 5, 6, 10, 12, 13, 14, 15)]
# Write preprocessed result
write.csv(processed, './datasets/full.processed.csv', row.names = F)

# EDA: Expoling the Data

# Load preprocessed data
processed <- read.csv('./datasets/full.processed.csv')
processed
head(processed)
train <- processed[!is.na(processed$Survived), ]
test <- processed[is.na(processed$Survived), ]

# Pclass .vs. Survived
ggplot(train, 
       aes(x = Pclass, 
           fill = factor(Survived))) +
    geom_histogram(stat = 'count')

tapply(train$Survived, train$Pclass, mean)

# NumCabin .vs. Survived
ggplot(train, 
       aes(x = NumCabin, 
           fill = factor(Survived))) +
    geom_histogram(stat = 'count')

tapply(train$Survived, train$NumCabin, mean)

# Gender .vs. Survived
ggplot(train, 
       aes(x = Gender, 
           fill = factor(Survived))) +
    geom_histogram(stat = 'count')

tapply(train$Survived, train$Gender, mean)

# Title .vs. Survived
ggplot(train, 
       aes(x = Title, 
           fill = factor(Survived))) +
    geom_histogram(stat = 'count')

tapply(train$Survived, train$Title, mean)

# NumFamily .vs. Survived
ggplot(train, 
       aes(x = NumFamily, 
           fill = factor(Survived))) +
    geom_histogram(stat = 'count')

tapply(train$Survived, train$NumFamily, mean)

# Embarked .vs. Survived
ggplot(train, 
       aes(x = Embarked, 
           fill = factor(Survived))) +
    geom_histogram(stat = 'count')

tapply(train$Survived, train$Embarked, mean)

# Age .vs. Survived
ggplot(train, 
       aes(x = Age, 
           fill = factor(Survived))) +
    geom_histogram()

tapply(train$Age, train$NumFamily, mean)

# age interval
intervals <- hist(train$Age, 10, plot = F)
intervals
cuts <- cut(train$Age, intervals$breaks)
cuts
rate <- tapply(train$Survived, cuts, mean)
rate
df1 <- data.frame(names(rate), rate)
df1

barplot(df1$rate, xlab = 'Age', ylab = 'survival rate')

# Fare .vs. Survived
ggplot(train, 
       aes(x = Fare, 
           fill = factor(Survived))) +
    geom_histogram()

tapply(train$Age, train$Fare, mean)

# age interval
intervals <- hist(train$Fare, 10, plot = F)
intervals
cuts <- cut(train$Fare, intervals$breaks)
cuts
rate <- tapply(train$Survived, cuts, mean)
rate
df1 <- data.frame(names(rate), rate)
df1

barplot(df1$rate, xlab = 'Fare', ylab = 'survival rate')

# Feature Engineering

# Load preprocessed data
processed <- read.csv('./datasets/full.processed.csv')
processed
head(processed)
train <- processed[!is.na(processed$Survived), ]

# Logistic Regression
attach(train)
formul <- factor(Survived) ~ 
    Pclass + Title + NumFamily + Gender + Fare
fit_logit <- glm(formula = formul,
                 data = train, 
                 family = binomial)
fit_logit

pred <- round(fit_logit$fitted.values)
pred
mean(train$Survived == pred)
table(train$Survived)
table(pred)

TP <- sum(train$Survived == 1 & pred == 1)
TN <- sum(train$Survived == 0 & pred == 0)
FP <- sum(train$Survived == 0 & pred == 1)
FN <- sum(train$Survived == 1 & pred == 0)
conf_matrix <- data.frame(c(TN, FN), c(FP, TP), 
                          row.names = c('Truth.F', 'Truth.T'))
names(conf_matrix) <- c('Predict.N', 'Predict.P')
conf_matrix

# 정확도
accuracy <- (TP + TN) / (TP + FP + TN + FN)
./datasets/
    
# 정밀도
precision <- TP / (TP + FP)
precision

# 민감도(재현율)
recall <- TP / (TP + FN)
sensitivity <- recall
recall

# 특정율
specificity <- TN / (TN + FP)
specificity

# F1 점수
F1_score <- 2 * precision * recall / (precision + recall)
F1_score

