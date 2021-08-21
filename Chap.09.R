
##### 09-1

library(ggmap)
myGoogleMapAPIKey='AIza......Z32E'
register_google(key=myGoogleMapAPIKey)

gc <- geocode(enc2utf8("복현동"))
gc
cen <- as.numeric(gc)
cen
map <- get_googlemap(center=cen)

ggmap(map)

##### 09-2

gc <- geocode(enc2utf8("설악산"))
cen <- as.numeric(gc)
map <- get_googlemap(center=cen,
                     zoom=9,
                     size=c(640,640),
                     maptype="roadmap")

ggmap(map)

##### 09-3

cen <- c(-118.233248, 34.085015)
map <- get_googlemap(center=cen)
ggmap(map)

##### 09-4

gc <- geocode(enc2utf8("경북대"))
cen <- as.numeric(gc)
map <- get_googlemap(center=cen,
                     zoom = 15,
                     maptype="roadmap",
                     marker=gc)

ggmap(map)

##### 09-5

library(ggplot2)
names <- c("용두암","성산일출봉","정방폭포", 
           "중문관광단지","한라산 1100고지","차귀도")
addr <- c("제주시 용두암길 15",
          "서귀포시 성산읍 성산리",
          "서귀포시 동홍동 299-3",
          "서귀포시 중문동 2624-1",
          "서귀포시 색달동 산1-2",
          "제주시 한경면 고산리 125")

gc <- geocode(enc2utf8(addr))
gc
df <- data.frame(name=names, lon=gc$lon, lat=gc$lat)
df
cen <- c(mean(df$lon),mean(df$lat))
map <- get_googlemap(center=cen,
                     maptype="roadmap",
                     zoom=10,
                     size=c(640,640),
                     marker=gc)

ggmap(map)

gmap <- ggmap(map)
gmap + geom_text(data=df,
                 aes(x=lon,y=lat),
                 size=5,
                 label=df$name)

##### 09-6

sp <- sample(1:nrow(wind),50)
df <- wind[sp,]
head(df)
cen <- c(mean(df$lon), mean(df$lat))
gc <- data.frame(lon=df$lon, lat=df$lat)
head(gc)
map <- get_googlemap(center=cen,
                     maptype="roadmap",
                     zoom=6,
                     marker=gc)

ggmap(map)

map <- get_googlemap(center=cen,
                     maptype="roadmap",
                     zoom=6)

gmap <- ggmap(map)

gmap + geom_point(data=df,
                  aes(x=lon,y=lat,size=spd),
                  alpha=0.5,
                  col="blue") +
  scale_size_continuous(range = c(1, 14))
