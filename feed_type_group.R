library(Matching)
library(survey)
library(reshape2)
library(tidyverse)
library(compareGroups)
library(readxl)
library(Hmisc)
library(xlsx)

#导入数据，2张表，一张是出生时，一张是出生后随访
QS11<-read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                 sheet = "QS11")
QS12<-read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                 sheet = "QS12")

#喂养分组的计算
#第一步把变量类型转换好
QS11<- QS11 %>%
  dplyr::mutate_at(.vars = vars(20,21), .fun =as.numeric ) 
QS12<- QS12%>%
  dplyr::mutate_at(.vars = vars(21,22), .fun =as.numeric ) 
QS12$`喂养情况(QS12FEED)`<-factor(QS12$`喂养情况(QS12FEED)`)

#计算原则
#所需随访时点：G1-V2（1月龄），G1-V3（3月龄），G1-V4（4月龄）.
#每个随访时点计算1次ratio，最终计算3次ratio的平均值。
#1）如果平均值为0，则为纯母乳喂养，
#2）如果平均值≥85%，则为纯奶粉喂养，
#3）如果85%>平均值>0，则为混合喂养。
#各个visit的ratio的计算方法一致，如下：
#如果喂养模式填写为纯母乳，则ratio记为0；若为纯奶粉，记为100%；若为混合喂养，则根据“每天奶粉喂养次数*喂养体积/780”进行计算。

#计算出生时刻的ratio
group_ration_1<- QS11 %>% 
  mutate(ratio= case_when(QS11$`宝宝出生后第一周到第一个月末，是否为纯母乳喂养?(QS11YN3)`=="是"~0,
                          QS11$`如果宝宝出生后不是纯母乳喂养，请说明喂养方式(QS11NEBF)`=="纯奶粉喂养"~1,
                          QS11$`如果宝宝出生后不是纯母乳喂养，请说明喂养方式(QS11NEBF)`=="混合喂养"~`每天 ___ 次(QS11FRQ3)`*`每次 ___ 毫升(QS11QUA1)`/780)) %>%
  select(受试者编号,受试者状态,数据节,ratio)

#计算后面随访时的ration
group_ration_2<- QS12 %>% mutate(ratio= case_when(QS12$`喂养情况(QS12FEED)`=="纯母乳"~0,
                                                  QS12$`喂养情况(QS12FEED)`=="纯奶粉"~1, 
                                                  QS12$`喂养情况(QS12FEED)`=="混合喂养"~QS12$`每天 ___ 次奶粉(QS12FRQ3)`*QS12$`每次 ___ 毫升(QS12QUA2)`/780)) %>%
  select(受试者编号,受试者状态,数据节,ratio)

#把两块合并，拼到底下，合成的表包括了出生后4月内喂养的数据
group_ration<-rbind(group_ration_1,group_ration_2)

#计算最后真正的mean of ratio
group<- group_ration %>% 
  filter(受试者状态=="入组") %>%  #退出的人删除
  filter(str_detect(数据节,c("1月龄|3月龄|4月龄"))) %>%  #把计算纳入的时刻筛选出来
  group_by(受试者编号) %>% 
  mutate(final_ratio= mean(ratio)) %>%
  dplyr::mutate(n=n()) %>%
  mutate(feeding_type=case_when(final_ratio==0~"BF",
                                final_ratio>0.85~"FF",
                                final_ratio>0 &final_ratio<0.85~"MF")) 

#CHN001-100046被去除因为没有第一次数据
#CHN001-100061分组有问题，因为3月龄写的混合喂养，但是没有写奶粉的量和次数


#导出
write.table(group,file="C:/Users/zhaiqiangrong/Desktop/雀巢/group.csv",sep=",",fileEncoding="GBK",row.names = F)

#group2是为元信息表做准备，去重
group2<-group[,c(1,7)] %>%
  distinct(受试者编号,.keep_all = T) 
names(group2)[1]<-"id"