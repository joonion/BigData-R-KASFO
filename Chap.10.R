##### KoNLP 설치하기

install.packages("KoNLP", 
                 repos = c("https://forkonlp.r-universe.dev",
                           "https://cloud.r-project.org"), 
                 INSTALL_opts = c("–no-multiarch") )



##### 10-1

Sys.setenv(JAVA_HOME='C:/java/jdk-14')

library(wordcloud)
library(KoNLP)
library(RColorBrewer)

getwd()

text <- readLines("./datasets/mis_document.txt", encoding ="UTF-8" )
buildDictionary(ext_dic = "woorimalsam")
pal2 <- brewer.pal(8, "Dark2")
noun <- sapply(text,extractNoun, USE.NAMES=F)
noun

##### 10-2

noun2 <- unlist(noun)
wordcount <- table(noun2)
temp <- sort(wordcount, decreasing=T)[1:10]
temp

temp <- temp[-1]
barplot(temp,
        names.arg = names(temp),
        col ="lightblue",
        main ="빈도수 높은 단어", 
        ylab = "단어 빈도수")


##### 10-3

wordcloud(names(wordcount),
          freq=wordcount,
          scale=c(6,0.7),
          min.freq=3,
          random.order=F,
          rot.per=.1,
          colors=pal2)

##### 10-4

buildDictionary(ext_dic = "woorimalsam",
                user_dic=data.frame("정치", "ncn"),
                replace_usr_dic = T)

noun <- sapply(text,extractNoun, USE.NAMES=F)
noun2 <- unlist(noun)

noun2 <- noun2[nchar(noun2)>1]
noun2 <- gsub("하지","", noun2)
noun2 <- gsub("때문","", noun2)

wordcount <- table(noun2)
wordcloud(names(wordcount),
          freq=wordcount,
          scale=c(6,0.7),
          min.freq=3,
          random.order=F,
          rot.per=.1,
          colors=pal2)
