library(readxl)
library(tidyverse)


x10_nestle <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/1st批次10管母乳出库（from 天津 to 北京雀巢）用于HMO检测.xlsx")
x64_nestle <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2nd批次64管母乳出库（from 天津 to 北京雀巢）用于HMO检测.xlsx")
x66_nestle <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/3rd批次66管母乳出库（from 天津 to 北京雀巢）用于HMO检测.xlsx")
x66_nestle <- x66_nestle[,-6]
names(x66_nestle)[4]<-"采集周期"
names(x66_nestle)[5]<-"采集时间"

x74_nestle <- rbind(x10_nestle,x64_nestle)
x140_nestle <- rbind(x74_nestle,x66_nestle)


X109 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/母乳样本/109.xlsx")

X109$样本编号 <-  substr(X109$样本编号,start = 1,stop = 11)
x140_nestle$样本编号 <- substr(x140_nestle$样本编号,start = 1,stop = 11)

check1 <- x140_nestle %>%  group_by(样本编号) %>%
  dplyr::mutate(n=n())

new <- x140_nestle %>% full_join(X109 ,by="样本编号")
write.table(new,file="C:/Users/zhaiqiangrong/Desktop/雀巢/milk.csv",sep=",",fileEncoding="GBK",row.names = F)
