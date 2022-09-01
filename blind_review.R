library(tableone)
library(readxl)
library(tidyverse)



#粪便样本采集数据集  对应co-primary endpoints-Microbiome maturation trajectory
LB3 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                  sheet = "LB_SAMP3")
#是否进行了婴儿调查问卷 对应co-primary endpoints-infant questionnaires
QS9 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                 sheet = "QS9")
#骨密度数据集 对应co-primary endpoints-SoS measures of tibia
sos <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                  sheet = "MO_SOS")
#胫骨桡骨测量  对应co-primary endpoints-Length of tibia bone
FA <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                 sheet = "FA")
#是否进行母乳采集  对应co-primary endpoints- Levels of HMOs
LB_RES <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                  sheet = "LB_RES")

#原则筛选这五个数据集里面“是”或非缺失部分
LB3 <- LB3 %>% 
  filter(`是否进行了粪便样本采集?(LB3PERF)`=="是") %>% 
  select(受试者编号,数据节)

QS9 <- QS9 %>%
  filter(`是否进行了 [婴儿健康调查问卷] ?(QS9PERF)`== "是") %>%
  select(受试者编号,数据节)

sos <- sos %>% 
  mutate_at(.vars = vars(17),.funs = as.numeric) %>%
  filter(is.na(`结果(MOORRES)`)=="FALSE") %>%
  select(受试者编号,数据节)

FA <- FA %>%
  mutate_at(.vars = vars(17),.funs = as.numeric) %>%
  filter(is.na(`结果(FAORRES)`)== "FALSE") %>%
  select(受试者编号,数据节)

LB_RES <- LB_RES %>%
  filter(`是否进行了母乳样本采集?(LBPERF)`== "是") %>%
  select(受试者编号,数据节)

FAS <- rbind(LB3,QS9)
FAS <- rbind(FAS,sos)
FAS <- rbind(FAS,FA)
FAS <- rbind(FAS,LB_RES)


fas1 <- FAS %>% group_by(受试者编号) %>%
               filter(str_detect(受试者编号,"CHN001-100")) %>%
               mutate(score1=case_when(str_detect(数据节,"G1-V1")~1)) %>%
               mutate(score2=case_when(str_detect(数据节,"G1-V2")~1)) %>%
               mutate(score3=case_when(str_detect(数据节,"G1-V3")~1)) %>%
               mutate(score4=case_when(str_detect(数据节,"G1-V4")~1)) %>%
               mutate(score5=case_when(str_detect(数据节,"G1-V5")~1)) %>%
               dplyr::mutate_at(.vars = vars(3:7), .fun = function(x)ifelse(is.na(x),0,x)) %>%
               mutate(score = max(score1)+max(score2)+max(score3)+
                              max(score4)+max(score5)) %>%
               filter(score==5) %>% distinct(受试者编号)
  
fas2 <- FAS %>% group_by(受试者编号) %>%
  filter(str_detect(受试者编号,"CHN001-200")) %>%
  mutate(score1=case_when(str_detect(数据节,"G2-V1")~1)) %>%
  mutate(score2=case_when(str_detect(数据节,"G2-V2")~1)) %>%
  mutate(score3=case_when(str_detect(数据节,"G2-V3")~1)) %>%
  dplyr::mutate_at(.vars = vars(3:5), .fun = function(x)ifelse(is.na(x),0,x)) %>%
  mutate(score = max(score1)+max(score2)+max(score3)) %>%
  filter(score==3) %>% distinct(受试者编号)

enrolled <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                       sheet = "DM")
enrollY <- enrolled %>% filter(受试者状态=="入组") %>% 
           summary(受试者编号)
enrollN <- enrolled %>% filter(受试者状态=="提前退出") %>%
           summary(受试者编号)


