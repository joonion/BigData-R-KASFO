
##### 08-1

library(treemap)

data(GNI2014)
head(GNI2014)

treemap(GNI2014,
        index=c("continent","iso3"),
        vSize="population",
        vColor="GNI",
        type="value",
        bg.labels="yellow",
        title="World's GNI")

##### 08-2

library(treemap)
st <- data.frame(state.x77)
st <- data.frame(st, stname=rownames(st))

treemap(st,
        index=c("stname"),
        vSize="Area",
        vColor="Income",
        type="value",
        title="USA states area and income" )

##### 08-3

st <- data.frame(state.x77)
symbols(st$Illiteracy, st$Murder,
        circles=st$Population,
        inches=0.3,
        fg="white",
        bg="lightgray",
        lwd=1.5,
        xlab="rate of Illiteracy",
        ylab="crime(murder) rate",
        main="Illiteracy and Crime")

text(st$Illiteracy, st$Murder,
     rownames(st),
     cex=0.6,
     col="brown")

##### 08-4

head(mtcars)

mosaicplot(~gear+vs, data = mtcars, color=TRUE,
           main ="Gear and Vs")

##### 08-5

library(ggplot2)

month <- c(1,2,3,4,5,6)
rain  <- c(55,50,45,50,60,70)
df <- data.frame(month,rain)
df

ggplot(df, aes(x=month,y=rain)) +
  geom_bar(stat="identity",
           width=0.7,
           fill="steelblue")

##### 08-6

ggplot(df, aes(x=month,y=rain)) +
  geom_bar(stat="identity",
           width=0.7,
           fill="steelblue") +
  ggtitle("월별 강수량") +
  theme(plot.title = element_text(size=25, face="bold", colour="steelblue")) +
  labs(x="월",y="강수량") +
  coord_flip()

##### 08-7

library(ggplot2)

ggplot(iris, aes(x=Petal.Length)) +
  geom_histogram(binwidth=0.5)

##### 08-8

library(ggplot2)

ggplot(iris, aes(x=Sepal.Width, fill=Species, color=Species)) +
  geom_histogram(binwidth = 0.5, position="dodge") +
  theme(legend.position="top")

##### 08-9

library(ggplot2)

ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point()

##### 08-10

library(ggplot2)

ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width,
                      color=Species)) +
  geom_point(size=3) +
  ggtitle("꽃잎의 길이와 폭") +
  theme(plot.title = element_text(size=25, face="bold", colour="steelblue"))

##### 08-11

library(ggplot2)

ggplot(data=iris, aes(y=Petal.Length)) +
  geom_boxplot(fill="yellow")

##### 08-12

library(ggplot2)

ggplot(data=iris, aes(y=Petal.Length, fill=Species)) +
  geom_boxplot()

##### 08-13

library(ggplot2)

year <- 1937:1960
cnt <- as.vector(airmiles)
df <- data.frame(year,cnt)
head(df)

ggplot(data=df, aes(x=year,y=cnt)) +
  geom_line(col="red")

##### 08-14

library(Rtsne)
library(ggplot2)

ds <- iris[,-5]

dup = which(duplicated(ds))
dup

ds <- ds[-dup,]
ds.y <- iris$Species[-dup]

tsne <- Rtsne(ds,dims=2, perplexity=10)

df.tsne <- data.frame(tsne$Y)
head(df.tsne)

ggplot(df.tsne, aes(x=X1, y=X2, color=ds.y)) +
  geom_point(size=2)

##### 08-15

#install.packages(c("rgl", "car"))

library("car")
library("rgl")

tsne <- Rtsne(ds,dims=3, perplexity=10)
df.tsne <- data.frame(tsne$Y)
head(df.tsne)

scatter3d(x=df.tsne$X1, y=df.tsne$X2, z=df.tsne$X3)

points <- as.integer(ds.y)
color <- c('red','green','blue')
scatter3d(x=df.tsne$X1, y=df.tsne$X2, z=df.tsne$X3,
          point.col = color[points],
          surface=FALSE)
