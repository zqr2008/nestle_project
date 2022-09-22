library(reshape2)
library(reshape)
library(tidyverse)
library(readxl)
library(Hmisc)
library(anthro)
library(conflicted)
library(labelled)


#加载三张表，gender主要包含孩子性别信息，QS包含母亲的教育、孩子数量
#wei_and_hei包含孩子各个时间点的身高体重
gender <- as.data.frame(listB[["DM"]])
wei_and_hei <- as.data.frame(listB[["VS"]])
QS2<-as.data.frame(listB[["QS2"]])
MH <- as.data.frame(listB[["MH"]])

#民族的字符串统一
gender2 <- gender %>% 
  dplyr::rename(delivery_mode = DELMETH)  %>%
  mutate(eth=case_when(str_detect(ETHNICITY,"Han") ~ "Han",
                                         str_detect(ETHNICITY,"满") ~ "Man",
                                         str_detect(ETHNICITY,"蒙古") ~ "Mongol",
                                         str_detect(ETHNICITY,"回")~ "Hui",
                                         str_detect(ETHNICITY,"朝鲜")~ "korean_chinese")) %>%
  mutate_at(.vars = vars(18), .funs = as.factor) %>%
  select(SubjectNo,GROUPING,SEX,delivery_mode,GESTAGE,eth) 

#把性别统一编码
gender2$SEX[trimws(gender2$SEX) == "Male"]<-"1"
gender2$SEX[trimws(gender2$SEX) == "Female"]<-"2"

#身高体重等信息整
wei_and_hei <- wei_and_hei %>% 
               dplyr::filter(str_detect(Instance,"G1-V6")=="FALSE") %>%
               dplyr::mutate_at(.vars =vars(8,15), .fun=as.factor) %>%
               dplyr::mutate_at(.vars =vars(16), .fun=as.numeric) %>%
               dplyr::rename(param = VSTEST,
                             value = VSORRES,
                             time = Instance)

simple<-wei_and_hei[,c(3,8,15,16)]
simple<-as.data.frame(simple)
#利用cast函数将身高和体重分成两列     
simple2<-reshape::cast(simple,SubjectNo+time~param) 
#把身高体重和性别关联
##simple3数据是用来给z-score计算作准备

simple3<- simple2 %>% left_join(gender2,by="SubjectNo") %>% #把性别等人口学信息和身高体重关联起来
  mutate(bmi=(10000*体重/身高)/身高) %>% #bmi算出来
  mutate(time = case_when(str_detect(time,"birth")=="TRUE"~0,   #把随访时间换成天数，是计算z-score需要
                          str_detect(time,"1 month")=="TRUE"~30,
                          str_detect(time,"3 months")=="TRUE"~90,
                          str_detect(time,"4 months")=="TRUE"~120,
                          str_detect(time,"6 months")=="TRUE"~180,
                          str_detect(time,"9 months")=="TRUE"~270,
                          str_detect(time,"12 months")=="TRUE"~360))

#用who的包计算z-score
z_score<-with(simple3, 
              anthro_zscores( 
              sex = SEX, age = time, #age输入必须按照日期
              weight = 体重, lenhei = 身高
              ))  %>% dplyr::select(zlen,zwei,zwfl,zbmi)  #单独把挑出来

#母亲教育程度和孩子数量的表整理
#统一id名称
names(QS2)[16]<-"child_number"
names(QS2)[17]<-"education"
#只需要QS2里面的教育程度和孩子数量，把其他去重
QS2<- QS2 %>% select(SubjectNo,child_number,education) %>%
      distinct(SubjectNo,.keep_all = T)

summary1<- cbind(simple3,z_score) %>% #simple3代之前合成的基本信息表， z_score代表用anthro包计算的zscore矩阵
           left_join(group2,by="SubjectNo") %>% #group2是用group.R文件计算出的feeding type分组
           left_join(QS2,by="SubjectNo") %>%  #QS2是母亲的教育、孩子数量 
           mutate(health=case_when(trimws(delivery_mode) == "Vaginal" &  #按照定义把健康的挑出来
                                   feeding_type_2 == "BF" &
                                   abs(zlen)<2 &
                                   abs(zwei)<2 &
                                   abs(zwfl)<2 ~ "healthy",
                                   TRUE ~ "unhealthy"))  %>%
  mutate(Instance = case_when(time == 0~ "G1-V1 (birth + 10 days)",
                             time == 30~ "G1-V2 (1 month ± 15 days)",
                             time == 90~ "G1-V3 (3 months ± 15 days)",
                             time == 120~ "G1-V4 (4 months ± 15 days)",
                             time == 180 & GROUPING == " Group 1" ~ "G1-V5 (6 months ± 15 days)",
                             time == 180 & GROUPING == " Group 2" ~ "G2-V1 (6 months ± 15 days)",
                             time == 270~ "G2-V2 (9 months ± 15 days)",
                             time == 360~ "G2-V3 (12 months ± 15 days)")) %>%
  relocate(SubjectNo,Instance)  %>%
  dplyr::rename(height = 身高,
                weight = 体重)


summary2 <- remove_labels(summary1) 
summary2$PatientId <- substr(summary2$SubjectNo,start = 8,stop = 13)
summary2 <- merge_bone %>%
  left_join(summary2,by=c("PatientId","Instance")) %>% 
  dplyr::rename(tibia_sos =`胫骨 (左)sos`,
         radius_sos = `桡骨 (左)sos`,
         tibia_length = `胫骨 (左)length`,
         radius_length = `桡骨 (左)length`) %>%
  filter(Instance!="G1-V6 (9 months ± 15 days)")
summary2$SEX<-factor(summary2$SEX,levels = c("1","2"),
                     labels = c("male","female"))   

summary2_sos <- summary2
save(summary2_sos, file = "summary2_sos.rda")


summary3<- MH %>% dplyr:: filter( trimws(MHYN) == "Yes") %>%
  distinct(SubjectNo) %>% mutate(medical_history = "Yes") %>%
  right_join(summary1,by = "SubjectNo") %>% 
  mutate_at(.vars = vars(2), .funs =  function(x)ifelse(is.na(x),"No",x))

write.table(summary1,file="C:/Users/zhaiqiangrong/Desktop/雀巢/summary1.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(summary2,file="C:/Users/zhaiqiangrong/Desktop/雀巢/summary2.csv",sep=",",fileEncoding="GBK",row.names = F)

#以下为描述性函数，可以不看
#table1<-descrTable(~.,data=summary1)
#table2<-descrTable(feeding_type~.,data=summary1,method = NA)
#table3<-descrTable(delivery_mode~.,data=summary1,method = NA)
#导出
#export2csv(table1,"C:/Users/zhaiqiangrong/Desktop/雀巢/table1.csv",sep=",",fileEncoding="GBK")
#export2csv(table2,"C:/Users/zhaiqiangrong/Desktop/雀巢/table2.csv",sep=",",fileEncoding="GBK")
#write.table(simple4,file="C:/Users/zhaiqiangrong/Desktop/雀巢/anth.csv",sep=",",fileEncoding="GBK",row.names = F)
#此部分不作为输出，是供挑选逻辑错误使用，可以不看
temp<- simple3 %>% 
  arrange(SubjectNo,time) %>% group_by(SubjectNo) %>%
  dplyr::mutate(n=n()) %>% filter(n>=2) %>%
  mutate(hei_diff=tsibble::difference(身高,differences = 1)) %>%
  mutate(weight_diff=tsibble::difference(体重,differences = 1)) %>%
  filter(weight_diff==0  | hei_diff== 0)

