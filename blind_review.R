library(tableone)
library(readxl)
library(tidyverse)

enrolled <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                                              sheet = "DM")

sos <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                       sheet = "MO_SOS")

LB2 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                  sheet = "LB_SAMP2")

LB3 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                  sheet = "LB_SAMP3")

VS <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                  sheet = "VS")

sos <- sos %>% 
  mutate_at(.vars = vars(17),.funs = as.numeric) %>%
  filter(is.na(`结果(MOORRES)`)=="FALSE") %>%
  select(受试者编号,数据节)

LB2 <- LB2 %>% 
  filter(`是否进行了尿液样本采集?(LB2PERF)`=="是") %>% 
  select(受试者编号,数据节)

LB3 <- LB3 %>% 
  filter(`是否进行了粪便样本采集?(LB3PERF)`=="是") %>% 
  select(受试者编号,数据节)

VS <- VS %>%
  mutate_at(.vars = vars(16),.funs = as.numeric ) %>%
  filter(is.na(`结果(VSORRES)`)=="FALSE") %>%
  select(受试者编号,数据节)

FAS <- rbind(sos,LB2)
FAS <- rbind(FAS,LB3)
FAS <- rbind(FAS,VS)



fas <- FAS %>% group_by(受试者编号) %>%
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
  

screen<-summary(enrolled$受试者编号)
enrollY <- enrolled %>% filter(受试者状态=="入组") %>% 
           summary(受试者编号)
enrollN <- enrolled %>% filter(受试者状态=="提前退出") %>%
           summary(受试者编号)



disposition <- data.frame(BF=,MF=)