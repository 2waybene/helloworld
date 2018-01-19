##===============================
##  make_word_cloud.R
##  Jianying Li
##  For Thuy-Ai's project
##  For paper: NAR - 01112018
##===============================
library(wordcloud2)
library(tm)
library(SnowballC)
library(wordcloud)


##================================
##  Get data
##================================

setwd("x:/project2018/p53_manuscript/WordCloud/")
d <- read.csv("Jianying_wordcloud_for_4_or_all_20180110.csv")

d.4.wordcloud <- d[,c(1,3)]
colnames(d.4.wordcloud) <- c ("word", "freq")
table(d.4.wordcloud$freq)




##==================================================
##  Minimum four or more counts -- 123 genes
##==================================================
head(d.4.wordcloud)
table(d.4.wordcloud$freq)
d.4 <- d.4.wordcloud[which(d.4.wordcloud$freq >=4),]
table(d.4$freq)
d.4$freq <- d.4$freq-3

## with changing the "size", all 51 are visiable.

set.seed(1234)
wordcloud2(d.4, size = 0.4, minRotation = -pi/16, maxRotation = -pi/6, minSize = 1 , rotateRatio = 0)


png("wordcloud_cistrom_4s.png", width=1280,height=800)
wordcloud(d.4$word,d.4$freq, scale=c(8,.2),min.freq=1,
          max.words=Inf, random.order=FALSE, rot.per=0, colors=pal2)
dev.off()


##  emf does not work well with word cloud
library(devEMF)
emf("wordcloud_cistrom_4s.emf", width=1280,height=800)
wordcloud(d.4$word,d.4$freq, scale=c(8,.2),min.freq=1,
          max.words=Inf, random.order=FALSE, rot.per=0, colors=pal2)
dev.off()


##  Now, test all 943 genes

d <- read.csv("Jianying_wordcloud_for_all_cistrom_20180110.csv")

d.all.wordcloud <- d[,c(1,3)]
colnames(d.all.wordcloud) <- c ("word", "freq")
table(d.all.wordcloud$freq)


pal2 <- brewer.pal(8,"Dark2")
png("wordcloud_cistrom_all.png", width=1280,height=800)
wordcloud(d.all.wordcloud$word,d.all.wordcloud$freq, scale=c(8,.2),min.freq=1,
          max.words=Inf, random.order=FALSE, rot.per=0, colors=pal2)
dev.off()

png("wordcloud_cistrom_all_v1.png", width=1280,height=800)
wordcloud(d.all.wordcloud$word,d.all.wordcloud$freq, scale=c(8,.2),min.freq=1,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()

##==============================
##  let's get a ball shape
##==============================

table(d.all.wordcloud$freq)

d.mod <- d.all.wordcloud

d.mod <- d.mod[order(d.mod$freq, decreasing = TRUE),]
head(d.mod)
