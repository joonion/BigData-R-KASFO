# 한국사학진흥재단: 행복기숙사 공공데이터 분석

library(ggplot2)
library(ggmap)

getwd()

# 행복기숙사 운영현황 정보

df.1 <- read.csv("./datasets/한국사학진흥재단_행복기숙사 운영현황_20201020.csv")
df.1
str(df.1)

colnames(df.1)
unique(df.1$구분)

summary(df.1)

# 행복기숙사 사업 지원현황 정보

df.2 <- read.csv("./datasets/한국사학진흥재단_행복기숙사 사업 지원현황_20201120.csv")
df.2
str(df.2)

colnames(df.2)
unique(df.2$연도)

summary(df.2)

# 행복기숙사 운영현황 데이터 탐색

barplot(table(df.1$구분),
        main = "행복기숙사 구분 현황",
        xlab = "구분",
        ylab = "수용인원(명)",
        col = c("red", "blue", "yellow"))

hist(df.1$수용인원수.명.,
     main = "행복기숙사 수용인원 히스토그램",
     xlab = "수용인원수",
     ylab = "기숙사수",
     col = c("red", "yellow", "magenta", "blue"),
     border = "grey",
     las = 2,
     breaks = 10)

boxplot(df.1$X2인실.기숙사비.천원. ~ df.1$구분, 
        main = "기숙사 유형별 2인실 기숙사비",
        col = c("red", "yellow", "green"))

outliers <- boxplot.stats(df.1$X2인실.기숙사비.천원.)$out
outliers
df.1[df.1$X2인실.기숙사비.천원. %in% outliers, c(1, 2, 3, 5)]

str(df.1)

# 행복기숙사 지원현황 데이터 탐색

colnames(df.2)
df.3 <- df.2[, c(1, 3, 5, 4)]
head(df.3)
str(df.3)

agg <- aggregate(df.3, 
                 by = list(year = df.3$연도),
                 sum)
agg
summary(agg)

plot(agg$year, agg$총사업비.백만원., 
     main = "연도별 지원현황",
     type = "b",
     lty = 1,
     col = "red",
     xlab = "연도",
     ylab = "지원규모", 
     ylim = c(10000, 220000))
     
lines(agg$year, agg$연면적...,
      type = "b",
      lty = 2,
      col = "blue")
      
lines(agg$year, 10 * agg$수용인원.명., 
      type = "b",
      lty = 3,
      col = "magenta")

unique(df.2$진행현황)
df.3$진행현황 <- df.2$진행현황 
for (i in 1:nrow(df.3)) {
    df.3$진행현황[i] <- unlist(strsplit(df.3$진행현황[i], '중|완'))[1]
}
unique(df.3$진행현황)

head(df.3)
tail(df.3)
str(df.3)

ggplot(df.3, aes(x=진행현황,y=총사업비.백만원.)) +
    geom_bar(stat="identity",
             width=0.7,
             fill="steelblue") +
    ggtitle("진행현황별 총사업비") +
    theme(plot.title = element_text(size=25, face="bold", colour="steelblue")) +
    labs(x="진행현황", y="총사업비") +
    coord_flip()

# 행복기숙사 위치 지도시각화

myGoogleMapAPIKey <- "AIza......Z32E"
register_google(key = myGoogleMapAPIKey)

names <- df.1$기숙사명 
names

addrs <- df.1$기숙사명 
addrs

capas <- df.1$수용인원수.명.
capas

gc <- geocode(enc2utf8(addrs))
gc
cent <- c(mean(gc$lon), mean(gc$lat)-0.5)
cent

map <- get_googlemap(center = cent,
                     zoom = 7,
                     size = c(768, 768),
                     maptype = "roadmap",
                     marker = gc)

df <- data.frame(name = names, lon = gc$lon, lat = gc$lat)
ggmap(map) + 
    geom_text(data = df, 
              aes(x = lon, y = lat),
              size = 5,
              label = df$name)

map <- get_googlemap(center = cent,
                     zoom = 7,
                     size = c(768, 768),
                     maptype = "roadmap")

ggmap(map) + 
    geom_text(data = df, 
              aes(x = lon, y = lat),
              size = 5,
              label = df$name) +
    geom_point(data = df, 
               aes(x = lon, y = lat, size = capas),
               alpha = 0.5,
               col = "magenta") +
    scale_size_continuous(range = c(1, 15))
