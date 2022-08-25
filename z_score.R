library(tableone)
library(Matching)
library(survey)
library(reshape2)
library(reshape)
library(ggplot2)
library(tidyverse)
library(compareGroups)
library(readxl)
library(Hmisc)
library(xlsx)
library(anthro)
library(plyr)


#加载三张表，gender主要包含孩子性别信息，QS包含母亲的教育、孩子数量
#wei_and_hei包含孩子各个时间点的身高体重
gender <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                     sheet = "DM")
wei_and_hei <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                          sheet = "VS")
QS2<-read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
               sheet = "QS2")


#性别分组等信息整理
names(gender)[3]<-"id"
names(gender)[20]<-"grouping"
names(gender)[13]<-"gender"
names(gender)[18]<-"delivery_mode"

#民族的字符串统一
gender<- gender %>% mutate(eth=case_when(str_detect(`民族(ETHNICITY)`,"汉")~"汉族",
                                           str_detect(`民族(ETHNICITY)`,"满")~"满族",
                                           str_detect(`民族(ETHNICITY)`,"蒙古")~"蒙古族",
                                           str_detect(`民族(ETHNICITY)`,"回")~"回族"))
#去除无关信息，只保留需要的人口学参数
gender2<-gender[,c(3,20,13,18,21)]
#把性别统一编码
gender2$gender[gender2$gender=="男"]<-"1"
gender2$gender[gender2$gender=="女"]<-"2"


#身高体重等信息整理
wei_and_hei <- wei_and_hei %>%
  dplyr::mutate_at(.vars =vars(8,15), .fun=as.factor) #变量转换
wei_and_hei <- wei_and_hei %>%
  dplyr::mutate_at(.vars =vars(16), .fun=as.numeric) #数值型变量转换
#统一名称
names(wei_and_hei)[15]<-"param"
names(wei_and_hei)[16]<-"value"
names(wei_and_hei)[3]<-"id"
names(wei_and_hei)[8]<-"time"
#simple把身高体重主要数据提取出来
simple<-wei_and_hei[,c(3,8,15,16)]
simple<-as.data.frame(simple)
#利用cast函数将身高和体重分成两列     
simple2<-reshape::cast(simple,id+time~param) 
#把身高体重和性别关联
##simple3数据是用来给z-score计算作准备

simple3<- simple2 %>% left_join(gender2,by="id") %>% #把性别等人口学信息和身高体重关联起来
  mutate(bmi=(10000*体重/身高)/身高) %>% #bmi算出来
  mutate(time = case_when(str_detect(time,"出生")=="TRUE"~0,   #把随访时间换成天数，是计算z-score需要
                          str_detect(time,"1月龄")=="TRUE"~30,
                          str_detect(time,"3月龄")=="TRUE"~90,
                          str_detect(time,"4月龄")=="TRUE"~120,
                          str_detect(time,"6月龄")=="TRUE"~180,
                          str_detect(time,"9月龄")=="TRUE"~270,
                          str_detect(time,"12月龄")=="TRUE"~360,

                                                                                       ))
#此部分不作为输出，是供挑选逻辑错误使用，可以不看
temp<- simple3 %>% 
  arrange(id,time) %>% group_by(id) %>%
  dplyr::mutate(n=n()) %>% filter(n>=2) %>%
  mutate(hei_diff=tsibble::difference(身高,differences = 1)) %>%
  mutate(weight_diff=tsibble::difference(体重,differences = 1)) %>%
  filter(weight_diff==0  | hei_diff== 0)

#循环 索引多个数字比较


#用who的包计算z-score
z_score<-with(simple3, 
              anthro_zscores( 
              sex = gender, age = time, #age输入必须按照日期
              weight = 体重, lenhei = 身高
              ))  %>% dplyr::select(zlen,zwei,zwfl,zbmi)  #单独把挑出来




#母亲教育程度和孩子数量的表整理
#统一id名称
names(QS2)[3]<-"id"
names(QS2)[16]<-"child_number"
names(QS2)[17]<-"education"
#只需要QS2里面的教育程度和孩子数量，把其他去重
QS2<- QS2 %>% select(id,child_number,education) %>%
  distinct(id,.keep_all = T)
summary1<- cbind(simple3,z_score) %>% #simple3代之前合成的基本信息表， z_score代表用anthro包计算的zscore矩阵
  left_join(group2,by="id") %>% #group2是用group.R文件计算出的feeding type分组
  left_join(QS2,by="id") %>%  #QS2是母亲的教育、孩子数量 
  mutate(health=case_when(delivery_mode=="顺产" &  #按照定义把健康的挑出来
                            feeding_type=="BF" & 
                            abs(zlen)<2 &
                            abs(zwei)<2 &
                            abs(zwfl)<2 ~"healthy"))
#三张表合起来 

summary1$health[is.na(summary1$health)]<-"unhealthy"
        
#输出大表结果
write.table(summary1,file="C:/Users/zhaiqiangrong/Desktop/雀巢/summary1.csv",sep=",",fileEncoding="GBK",row.names = F)

#随访按照时间点输出结果
##设置不同时间点的list
list1<-list()
list1<-c(0,30,90,120,180,270,360)
name=paste("result",c(1:7),".csv",sep="")

##用循环把所有时间点的结果输出
##descrTable命令把各信息按照时间点描述数据
for (i in 1:7){
  x=summary1%>% filter(time==list1[i]) 
  table=descrTable(delivery_mode~.,data=x,method = NA)
  export2csv(table,name[i],sep=",",fileEncoding="GBK")
}




#以下为描述性函数，可以不看
#table1<-descrTable(~.,data=summary1)
#table2<-descrTable(feeding_type~.,data=summary1,method = NA)
#table3<-descrTable(delivery_mode~.,data=summary1,method = NA)
#导出
#export2csv(table1,"C:/Users/zhaiqiangrong/Desktop/雀巢/table1.csv",sep=",",fileEncoding="GBK")
#export2csv(table2,"C:/Users/zhaiqiangrong/Desktop/雀巢/table2.csv",sep=",",fileEncoding="GBK")
#write.table(simple4,file="C:/Users/zhaiqiangrong/Desktop/雀巢/anth.csv",sep=",",fileEncoding="GBK",row.names = F)


#at_birth<-summary1%>% filter(time==0)
#month_1<-summary1%>% filter(time==30)
#month_3<-summary1%>% filter(time==90)
#month_4<-summary1%>% filter(time==120)
#month_6<-summary1%>% filter(time==180)
#month_9<-summary1%>% filter(time==270)
#month_12<-summary1%>% filter(time==360)