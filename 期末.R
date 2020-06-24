rm(list=ls())
library(tidyverse)
library(ggplot2)
library(maps)
library(viridisLite)
library(viridis)
library(readr)
library(nCov2019)
#資料的讀取
data<-"/Users/wangmingru/who_covid_19_sit_rep_time_series.csv"
table<-read.csv(data,header = T)
data<-"/Users/wangmingru/time_series_covid19_confirmed_global.csv"
Confirmed<-read.csv(data,header = T)
#資料重整
wmr<-table[1:4,] #擷取特定質料：分別為全球確診、全球死亡、中國情況、中國以外的情況
Global_Confirmed<-table[1,]
Global_Death<-table[2,]
China_Confirmed<-table[3,]
China_Death<-table[5,]
Outside_China_Confirmed<-table[4,]
temp <- seq.Date(from = as.Date("2020/01/21",format = "%Y/%m/%d"), 
                 by = "day", length.out = 117) # 2020/1/21-2020/5/13
table<-t(table)
world<-map_data("world")
#
#資料視覺化
options(scipen = 10000000)
plot(x=c(temp),      # X軸的值
     y=Global_Confirmed,       # Y軸的值
     main="Global_Confirmed",    # 圖片名稱
     xlab="date",       # X軸的名稱
     ylab="people"  ,       # Y軸的名稱
     pch = 20,
     cex = 1 ,#大小
     col = "red"
)

plot(x=c(temp),      # X軸的值
     y=Global_Death,       # Y軸的值
     main="Global_Death",    # 圖片名稱
     xlab="date",       # X軸的名稱
     ylab="people"  ,       # Y軸的名稱
     pch = 20,
     cex = 1 ,#大小
     col = "red"
)

plot(x=c(temp),      # X軸的值
     y=China_Confirmed,       # Y軸的值
     main="China_Confirmed",    # 圖片名稱
     xlab="date",       # X軸的名稱
     ylab="people"  ,       # Y軸的名稱
     pch = 20,
     cex = 1 ,#大小
     col = "red"
)

plot(x=c(temp),      # X軸的值
     y=China_Death,       # Y軸的值
     main="China_Death",    # 圖片名稱
     xlab="date",       # X軸的名稱
     ylab="people"  ,       # Y軸的名稱
     pch = 20,
     cex = 1 ,#大小
     col = "red"
)

plot(x=c(temp),      # X軸的值
     y=Outside_China_Confirmed,       # Y軸的值
     main="Outside_China_Confirmed",    # 圖片名稱
     xlab="date",       # X軸的名稱
     ylab="people"  ,       # Y軸的名稱
     pch = 20,
     cex = 1 ,#大小
     col = "red"
)

#Compare
plot(x=c(temp),      # X軸的值
     y=Global_Confirmed,       # Y軸的值
     main="Compare_Global_Confirmed&Death",    # 圖片名稱
     xlab="date",       # X軸的名稱
     ylab="people"  ,       # Y軸的名稱
     pch = 20,
     cex = 0.5 ,#大小
     col = "red"
)
points(x=c(temp),      # X軸的值
       y=Global_Death,       # Y軸的值
       main="Global_Death",    # 圖片名稱
       xlab="date",       # X軸的名稱
       ylab="people"  ,       # Y軸的名稱
       pch = 4,
       cex = 0.5 ,#大小
       col = "blue"
)
legend("topleft", c("confirmed", "death"), col = c("red","blue"), 
       lty = c(2, -1), pch = c(20, 4), merge = TRUE, bg='gray90')
###china
plot(x=c(temp),      # X軸的值
     y=China_Confirmed,       # Y軸的值
     main="Compare_China_Confirmed&Death",    # 圖片名稱
     xlab="date",       # X軸的名稱
     ylab="people"  ,       # Y軸的名稱
     pch = 20,
     cex = 0.5 ,#大小
     col = "red"
)
points(x=c(temp),      # X軸的值
       y=China_Death,       # Y軸的值
       main="Global_Death",    # 圖片名稱
       xlab="date",       # X軸的名稱
       ylab="people"  ,       # Y軸的名稱
       pch = 4,
       cex = 0.5 ,#大小
       col = "blue"
)
legend("topleft", c("confirmed", "death"), col = c("red","blue"), 
       lty = c(2, -1), pch = c(20, 4), merge = TRUE, bg='gray90')

plot(x=c(temp),      # X軸的值
     y=China_Confirmed,       # Y軸的值
     main="Compare_China_Confirmed&Outside_China_Confirmed",    # 圖片名稱
     xlab="date",       # X軸的名稱
     ylab="people"  ,       # Y軸的名稱
     pch = 20,
     cex = 0.5 ,#大小
     col = "red"
)
points(x=c(temp),      # X軸的值
       y=Outside_China_Confirmed,       # Y軸的值
       main="Global_Death",    # 圖片名稱
       xlab="date",       # X軸的名稱
       ylab="people"  ,       # Y軸的名稱
       pch = 4,
       cex = 0.5 ,#大小
       col = "blue"
)
legend("topleft", c("confirmed", "death"), col = c("red","blue"), 
       lty = c(2, -1), pch = c(20, 4), merge = TRUE, bg='gray90')
#世界地圖
ggplot()+
        geom_polygon(data=world,aes(x=long,y=lat,group=group),fill="grey",alpha=0.3) +
        geom_point(data=Confirmed,aes(x=Long,y=Lat,size=`3/15/20`,color=`3/15/20`),alpha=0.5)
#進階
mybreaks<- c(1, 20, 100, 1000, 50000)
mylabels<- c("1-19", "20-99", "100-999","1,000-49,999", "50,000+")
ggplot() +
        geom_polygon(data=world, aes(x=long, y=lat,group=group), fill="grey", alpha=0.3) +
        geom_point(data=Confirmed, aes(x=Long, y=Lat,size=`3/15/20`, color=`3/15/20`), alpha=0.5) +
        scale_size_continuous(name="Confirmedcases", trans="log", range=c(1,7), breaks=mybreaks,labels=mylabels) +
        scale_colour_viridis_c(option="inferno", direction=-1,name="Confirmed cases", trans="log", breaks=mybreaks,labels=mylabels) +
        guides(colour=guide_legend()) +
        theme_void() +
        theme(legend.position="bottom")