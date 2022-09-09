library(Matching)
library(survey)
library(reshape2)
library(tidyverse)
library(compareGroups)
library(readxl)
library(Hmisc)
library(xlsx)
library(daff)

#导入数据，每个sheet都有2张表，一张是以变量代码呈现，一张是以变量标签名呈现
QS11 <- as.data.frame(listB[["QS11"]])
QS12 <- as.data.frame(listB[["QS12"]])

#喂养分组的计算
#第一步把变量类型转换好
QS11<- QS11 %>%
  dplyr::mutate_at(.vars = vars(20,21), .fun =as.numeric ) %>%
  rename(formula_brand = QS11BRA,
         probiotics = QS11CON1,
         prebiotics_fiber = QS11CON2,
         hydrolyzed_protein = QS11CON3,
         formula_content = QS11INGR)

QS12<- QS12%>% 
  dplyr::mutate_at(.vars = vars(21,22), .fun =as.numeric ) %>%
  dplyr::mutate_at(.vars = vars(13), .fun =as.factor ) %>%
  rename(formula_brand = QS12BRA,
         probiotics = QS12CON1,
         prebiotics_fiber = QS12CON2,
         hydrolyzed_protein = QS12CON3,
         formula_content = QS12INGR)

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
  mutate(ratio= case_when(trimws(QS11YN3) == "Yes"~0,
                          trimws(QS11NEBF) == "Fed with milk powder only"~1,
                          trimws(QS11NEBF) == "Mixed feeding"~ (QS11FRQ3*QS11QUA1)/780)) %>%
  mutate(formula_feeding = case_when(trimws(QS11NEBF) == "Fed with milk powder only" ~"Yes",
                                    TRUE ~ "No"))%>%
  select(SubjectNo,SubjectStatus,Instance,formula_feeding,ratio,formula_brand,
         probiotics,prebiotics_fiber,hydrolyzed_protein,formula_content)

#计算后面随访时的ration
group_ration_2<- QS12 %>% mutate(ratio= case_when(trimws(QS12FEED) == "Exclusively with breast milk"~0,
                                                  trimws(QS12FEED) == "Exclusively with milk powder"~1, 
                                                  trimws(QS12FEED) =="Mixed feeding"~QS12FRQ3*QS12QUA2/780)) %>%
  mutate(formula_feeding = case_when(trimws(QS12FEED) == "Exclusively with milk powder"~ "Yes",
                                    TRUE ~ "No"
                                    ))  %>%
  select(SubjectNo,SubjectStatus,Instance,formula_feeding,ratio,formula_brand,
         probiotics,prebiotics_fiber,hydrolyzed_protein,formula_content)

#把两块合并，拼到底下，合成的表包括了出生后4月内喂养的数据
group_ration <- rbind(group_ration_1,group_ration_2)

#计算最后真正的mean of ratio
group<- group_ration %>% 
  filter(trimws(SubjectStatus) == "Enrolled") %>%
  filter(str_detect(SubjectNo,"CHN001-100")) %>%
  filter(str_detect(Instance,c("1 month|3 months|4 months"))) %>%  #把计算纳入的时刻筛选出来
  group_by(SubjectNo) %>% 
  mutate(final_ratio = mean(ratio)) %>%
  dplyr::mutate(n=n()) %>%
  mutate(feeding_type = case_when(final_ratio == 0~"BF",
                                  final_ratio > 0.85~"FF",
                                  final_ratio > 0 & final_ratio<0.85~"MF")) %>%
  mutate(feeding_type_remark = case_when(feeding_type == "FF" & trimws(ratio) < 0.85 ~"questionable")) %>%
  ungroup() %>%
  group_by(SubjectNo,feeding_type) %>%
  mutate(feeding_type_2 = case_when(feeding_type == "BF" ~ "BF",
                                    feeding_type == "FF" & any(str_detect(feeding_type_remark,"questionable")) ~ "MF",
                                    feeding_type == "FF" & is.na(feeding_type_remark) == "TRUE" ~ "FF",
                                    feeding_type == "MF" ~ "MF"))


#导出
write.table(group2,file="C:/Users/zhaiqiangrong/Desktop/雀巢/group.csv",sep=",",fileEncoding="GBK",row.names = F)

#group2是为元信息表做准备，去重
group2<-group%>%
  distinct(SubjectNo,.keep_all = TRUE)  