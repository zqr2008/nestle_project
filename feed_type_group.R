library(Matching)
library(survey)
library(reshape2)
library(tidyverse)
library(compareGroups)
library(readxl)
library(Hmisc)
library(xlsx)

#导入数据，每个sheet都有2张表，一张是以变量代码呈现，一张是以变量标签名呈现
QS11 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220831/2027NRC_FormExcel_3.0_20220831（以变量代码呈现）.xlsx", 
                 sheet = "QS11")
QS11_label <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220831/2027NRC_FormExcel_3.0_20220831（以变量标签名呈现）.xlsx", 
                         sheet = "QS11",col_names = F)
QS12 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220831/2027NRC_FormExcel_3.0_20220831（以变量代码呈现）.xlsx", 
                   sheet = "QS12")
QS12_label <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220831/2027NRC_FormExcel_3.0_20220831（以变量标签名呈现）.xlsx", 
                         sheet = "QS12",col_names = F)

#第一个输入文件QS11是一张是以变量代码呈现的sheet,第二个输入文件QS11_label是以变量标签名呈现的sheet
#一定要保证输入的顺序，同时两个文件是严格对应sheet
QS11 <- function_transfer(QS11,QS11_label)
QS12 <- function_transfer(QS12,QS12_label)


#喂养分组的计算
#第一步把变量类型转换好
QS11<- QS11 %>%
  dplyr::mutate_at(.vars = vars(20,21), .fun =as.numeric ) %>%
  rename(formula_brand = `奶粉，品牌名称(QS11BRA)`,
         probiotics = `它是否含有益生菌?(QS11CON1)`,
         prebiotics_fiber = `它是否含有益生元/纤维?(QS11CON2)`,
         hydrolyzed_protein = `它是否含有水解蛋白?(QS11CON3)`,
         formula_content = `奶粉是基于哪种原料生产的?(QS11INGR)`)

QS12<- QS12%>% 
  dplyr::mutate_at(.vars = vars(21,22), .fun =as.numeric ) %>%
  dplyr::mutate_at(.vars = vars(13), .fun =as.factor ) %>%
  rename(formula_brand = `奶粉，品牌名称(QS12BRA)`,
         probiotics = `它是否含有益生菌?(QS12CON1)`,
         prebiotics_fiber = `它是否含有低聚糖/益生元/纤维?(QS12CON2)`,
         hydrolyzed_protein = `它是否含有水解蛋白?(QS12CON3)`,
         formula_content = `奶粉是基于哪种原料生产的?(QS12INGR)`)

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
  select(受试者编号,受试者状态,数据节,ratio,formula_brand,
         probiotics,prebiotics_fiber,hydrolyzed_protein,formula_content)

#计算后面随访时的ration
group_ration_2<- QS12 %>% mutate(ratio= case_when(QS12$`喂养情况(QS12FEED)`=="纯母乳"~0,
                                                  QS12$`喂养情况(QS12FEED)`=="纯奶粉"~1, 
                                                  QS12$`喂养情况(QS12FEED)`=="混合喂养"~QS12$`每天 ___ 次奶粉(QS12FRQ3)`*QS12$`每次 ___ 毫升(QS12QUA2)`/780)) %>%
  select(受试者编号,受试者状态,数据节,ratio,formula_brand,
         probiotics,prebiotics_fiber,hydrolyzed_protein,formula_content)

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


#导出
write.table(group,file="C:/Users/zhaiqiangrong/Desktop/雀巢/group.csv",sep=",",fileEncoding="GBK",row.names = F)

#group2是为元信息表做准备，去重
group2<-group[,c(1,7)] %>%
  distinct(受试者编号,.keep_all = T) 
names(group2)[1]<-"id"