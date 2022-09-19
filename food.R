library(tidyverse)
library(readxl)
library(reshape)


food <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/FoodGroup-Interim-20220907.xlsx", 
           sheet = "FoodGroup-Interim-20220907")
raw <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/DUND - 102400_BAMBOO_PKU_Rawdata_final_V1_31JUL2022.xlsx")
names(raw)[8] <- "Food"
names(food)[5] <- "DDS"

me　<- raw %>% left_join(food, by =("Food")) %>%
  select(SUBJECT,VISIT,Food,DDS) %>% 
  group_by(SUBJECT,VISIT) %>%
  mutate_at(.vars = vars(4),.funs = as.character) %>%
  mutate(score1=case_when(str_detect(DDS,"1")~1)) %>%
  mutate(score2=case_when(str_detect(DDS,"2")~1)) %>%
  mutate(score3=case_when(str_detect(DDS,"3")~1)) %>%
  mutate(score4=case_when(str_detect(DDS,"4")~1)) %>%
  mutate(score5=case_when(str_detect(DDS,"5")~1)) %>%
  mutate(score6=case_when(str_detect(DDS,"6")~1)) %>%
  mutate(score7=case_when(str_detect(DDS,"7")~1)) %>%
  mutate(score8=case_when(str_detect(DDS,"8")~1)) %>%
  dplyr::mutate_at(.vars = vars(5:12), .fun = function(x)ifelse(is.na(x),0,x)) %>%
  mutate(DDS_score = max(score1)+max(score2)+max(score3)+
           max(score4)+max(score5)+max(score6)+max(score7)+
           max(score8)) %>%
  distinct(SUBJECT,VISIT,.keep_all = T) %>%
  select(SUBJECT,VISIT,DDS_score)

