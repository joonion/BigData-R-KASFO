
myGoogleMapAPIKey='AIza......Z32E'

##### 13-1

getwd()
library(ggplot2)
library(ggmap)
library(readxl)

files <- c("201512","201606","201612","201706","201712")
files

columns <- c( 1, 2, 5, 7, 9, 15, 17, 38, 39)
columns

ds.total <- NULL
ds.total

for (i in 1:length(files)) {
  filename <- paste("./datasets/", "seoul_", files[i], ".xlsx", sep="")
  cat("read ", filename, "...\n")

  ds <- read_excel(filename)
  ds <- data.frame(ds)
  ds <- ds[,columns]
  ds$수집연월 <- rep(i, nrow(ds))
  ds.total <- rbind(ds.total, ds)
}

head(ds.total)

##### 13-2

str(ds.total)

unique(ds.total$수집연월)
unique(ds.total$상권업종대분류명)
unique(ds.total$상권업종중분류명)
unique(ds.total$상권업종소분류명)

sum(is.na(ds.total))

ds.201712 <- subset(ds.total, ds.total$수집연월 == 5)
dim(ds.201712)
ds.201712
store.level_1 <- aggregate(ds.201712[,1],
                           by=list(대분류=ds.201712$상권업종대분류명),
                           FUN=length)
store.level_1

names(store.level_1)[2] = c("count")

ggplot(store.level_1, aes(x=대분류, y=count)) +
  geom_bar(stat="identity", width=0.7, fill="steelblue") +
  ggtitle("업종별 점포수") +
  theme(plot.title = element_text(color="black", size=14, face="bold"))

store.region <- aggregate(ds.201712[,1],
                          by=list(구이름=ds.201712$시군구명),
                          FUN=length)
store.region

names(store.region)[2] = c("count")

ggplot(store.region, aes(x=구이름, y=count)) +
  geom_bar(stat="identity", width=0.7, fill="steelblue") +
  ggtitle("구별 점포수") +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        axis.text.x = element_text(angle = 45))

store.region.loc <- aggregate(ds.201712[,c("경도","위도")],
                              by=list(구이름=ds.201712$시군구명),
                              FUN=mean)
store.region <- data.frame(store.region, store.region.loc[,2:3])

register_google(key=myGoogleMapAPIKey)

cen <- c(mean(store.region$경도),mean(store.region$위도))

map <- get_googlemap(center=cen,
                     maptype="roadmap",
                     size=c(640,640),
                     zoom=11)

gmap <- ggmap(map)

gmap+geom_point(data=store.region,
                aes(x=경도,y=위도,size=count),
                alpha=0.5, col="red") +
  scale_size_continuous(range = c(1, 15))+
  geom_text(data=store.region,
            aes(x=경도,y=위도),
            size=3,
            label=store.region$구이름)

store.dong <- aggregate(ds.201712[,1],
                        by=list(동이름=ds.201712$행정동명),
                        FUN=length)
store.dong

names(store.dong)[2] = c("count")
store.dong <- store.dong[order(store.dong$count,decreasing=T),]
dong.top10 <- store.dong[1:10,]
dong.top10

ggplot(dong.top10, aes(x=reorder(동이름, -count), y=count)) +
  geom_bar(stat="identity", width=0.7, fill="steelblue") +
  ggtitle("점포수 많은 상위 10개동") +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        axis.text.x = element_text(angle = 45))

##### 13-3

store.change <- aggregate(ds.total[,1],
                          by=list(연월=ds.total$수집연월,
                                  업종대분류=ds.total$상권업종대분류명),
                          FUN=length)
head(store.change)

names(store.change)[3] <- c("count")
ggplot(store.change, aes(x=연월, y=count, colour=업종대분류, group=업종대분류)) +
  geom_line() +
  geom_point(size=6, shape=19, alpha=0.5) +
  ggtitle("업종별 점포수 변화 (대분류)") +
  ylab("점포수") +
  scale_x_continuous(breaks=1:5,
                     labels=files) +
  theme(plot.title = element_text(color="black", size=14, face="bold"))

store.tmp <- aggregate(ds.total[,1],
                       by=list(연월=ds.total$수집연월,
                               업종소분류=ds.total$상권업종소분류명),
                       FUN=length)

names(store.tmp)[3] <- c("count")
store.201512 <- store.tmp[store.tmp$연월==1,]

names(store.201512)[3] <- c("cnt_2015")
store.201712 <- store.tmp[store.tmp$연월==5,]

names(store.201712)[3] <- c("cnt_2017")
store.diff <- merge(store.201512[,2:3], store.201712[,2:3])
store.diff$diff <- abs(store.diff$cnt_2015-store.diff$cnt_2017)
store.diff <- store.diff[order(by=store.diff$diff, decreasing=T),]

top10 <- store.diff[1:10,1]
top10

store.change <- subset(store.tmp, store.tmp$한글 %in% top10)

ggplot(store.change, aes(x=연월, y=count, colour=업종소분류, group=업종소분류)) +
  geom_line() +
  geom_point(size=6, shape=19, alpha=0.5) +
  ggtitle("점포수 변화 Top 10 업종(소분류)") +
  ylab("점포수수") +
  scale_x_continuous(breaks=1:5,
                     labels=files) +
  theme(plot.title = element_text(color="black", size=14, face="bold"))

store.gu <- aggregate(ds.total[,1],
                      by=list(연월=ds.total$수집연월,
                              구이름=ds.total$시군구명),
                      FUN=length)

names(store.gu)[3] <- c("count")
ggplot(store.gu, aes(x=연월, y=count, colour=구이름, group=구이름)) +
  geom_line() +
  geom_point(size=6, shape=19, alpha=0.5) +
  ggtitle("구별 점포수 변화 (대분류)") +
  ylab("점포수") +
  scale_x_continuous(breaks=1:5,
                     labels=files) +
  theme(plot.title = element_text(color="black", size=14, face="bold"))

store.tmp <- aggregate(ds.total[,1],
                       by=list(연월=ds.total$수집연월,
                               동이름=ds.total$행정동명),
                       FUN=length)

names(store.tmp)[3] <- c("count")

store.dong.201512 <- store.tmp[store.tmp$연월==1,]
names(store.dong.201512)[3] <- c("cnt_2015")

store.dong.201712 <- store.tmp[store.tmp$연월==5,]
names(store.dong.201712)[3] <- c("cnt_2017")

store.diff <- merge(store.dong.201512[,2:3], store.dong.201712[,2:3])
store.diff$diff <- abs(store.diff$cnt_2015-store.diff$cnt_2017)

store.diff <- store.diff[order(by=store.diff$diff, decreasing=T),]
top10 <- store.diff[1:10,1]
top10

store.change <- subset(store.tmp, store.tmp$동이름 %in% top10)

ggplot(store.change, aes(x=연월, y=count, colour=동이름, group=동이름)) +
  geom_line() +
  geom_point(size=6, shape=19, alpha=0.5) +
  ggtitle("점포수 변화 Top 10 동") +
  ylab("점포수수") +
  scale_x_continuous(breaks=1:5,
                     labels=files) +
  theme(plot.title = element_text(color="black", size=14, face="bold"))

##### 13-4

ds.yeoksam <- subset(ds.total, ds.total$수집연월==5 &
                       ds.total$행정동명=="역삼1동")

cen <- c(mean(ds.yeoksam$경도),mean(ds.yeoksam$위도))
map <- get_googlemap(center=cen,
                     maptype="roadmap",
                     size=c(640,640),
                     zoom=15)

gmap <- ggmap(map)
gmap+geom_point(data = ds.yeoksam,
                aes(x=경도,y=위도,color=상권업종대분류명),size=2,alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="역삼 1동 업종별 점포", color = "업종")

ds.yeoksam2 <- subset(ds.yeoksam, ds.yeoksam$상권업종소분류명==
                        "커피전문점/카페/다방")

gmap+geom_point(data = ds.yeoksam2,
                aes(x=경도,y=위도), size=2, alpha=0.5, col="red") +
  labs(x = "Longitude", y = "Latitude",
       title="역삼1동 커피점" )
