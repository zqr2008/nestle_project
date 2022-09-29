library(tableone)
library(readxl)
library(tidyverse)
library(conflicted)
library(daff)
library(gtsummary)
library(readxl)
library(labelled)

pd <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_PD listing_20220926_PD_review.xlsx")

filter <- dplyr::filter
#粪便样本采集数据集  对应co-primary endpoints-Microbiome maturation trajectory
LB3 <- as.data.frame(listB[["LB_SAMP3"]])
#是否进行了婴儿调查问卷 对应co-primary endpoints-infant questionnaires
QS9 <- as.data.frame(listB[["QS9"]])
#骨密度数据集 对应co-primary endpoints-SoS measures of tibia
sos <- as.data.frame(listB[["MO_SOS"]])
#胫骨桡骨测量  对应co-primary endpoints-Length of tibia bone
FA <- as.data.frame(listB[["FA"]])
#是否进行母乳采集  对应co-primary endpoints- Levels of HMOs
LB_RES <- as.data.frame(listB[["LB_RES"]])



pd <- pd %>% filter(`Impact analysis` == "Impact") %>%
  distinct(Subject) %>%
  rename(SubjectNo = Subject)


DM <- as.data.frame(listB[["DM"]])
table(DM$SubjectStatus)
DM <- DM %>% filter(SubjectStatus == " Enrolled") %>%
  select(SubjectNo) %>% left_join(group2, by ="SubjectNo")
 


LB3 <- LB3 %>% 
  dplyr::filter(trimws(LB3PERF)=="Yes") %>% 
  select(SubjectNo,Instance)

QS9 <- QS9 %>%
  filter(trimws(QS9PERF) == "Yes") %>%
  select(SubjectNo,Instance)

sos <- sos %>%
  mutate_at(.vars = vars(17),.funs = as.numeric) %>%
  filter(is.na(MOORRES) =="FALSE") %>%
  select(SubjectNo,Instance)

FA <- FA %>%
  mutate_at(.vars = vars(17),.funs = as.numeric) %>%
  filter(is.na(FAORRES) == "FALSE") %>%
  select(SubjectNo,Instance)

LB_RES <- LB_RES %>% 
  filter(trimws(LBPERF) == "Yes") %>%
  select(SubjectNo,Instance)


FAS <- rbind(LB3,QS9)
FAS <- rbind(FAS,sos)
FAS <- rbind(FAS,FA)
FAS <- rbind(FAS,LB_RES)


fas1 <- FAS %>% group_by(SubjectNo) %>%
               filter(str_detect(SubjectNo,"CHN001-100")) %>%
               mutate(score1=case_when(str_detect(Instance,"G1-V1")~1)) %>%
               mutate(score2=case_when(str_detect(Instance,"G1-V2")~1)) %>%
               mutate(score3=case_when(str_detect(Instance,"G1-V3")~1)) %>%
               mutate(score4=case_when(str_detect(Instance,"G1-V4")~1)) %>%
               mutate(score5=case_when(str_detect(Instance,"G1-V5")~1)) %>%
               dplyr::mutate_at(.vars = vars(3:7), .fun = function(x)ifelse(is.na(x),0,x)) %>%
               mutate(score = max(score1)+max(score2)+max(score3)+
                              max(score4)+max(score5)) %>%
               filter(score==5) %>% distinct(SubjectNo) %>%
  left_join(group2,by= "SubjectNo")

fas1 <- remove_labels(fas1) %>% mutate(SubjectNo=as.character(SubjectNo))
pp1 <- fas1 %>% anti_join(pd, by="SubjectNo")


fas2 <- FAS %>% group_by(SubjectNo) %>%
  filter(str_detect(SubjectNo,"CHN001-200")) %>%
  mutate(score1=case_when(str_detect(Instance,"G2-V1")~1)) %>%
  mutate(score2=case_when(str_detect(Instance,"G2-V2")~1)) %>%
  mutate(score3=case_when(str_detect(Instance,"G2-V3")~1)) %>%
  dplyr::mutate_at(.vars = vars(3:5), .fun = function(x)ifelse(is.na(x),0,x)) %>%
  mutate(score = max(score1)+max(score2)+max(score3)) %>%
  filter(score==3) %>% distinct(SubjectNo) 


fas2 <- remove_labels(fas2) %>% mutate(SubjectNo=as.character(SubjectNo))
pp2 <- fas2 %>% anti_join(pd, by="SubjectNo")

a<-as.data.frame(fas1$SubjectNo)
b<-as.data.frame(group2$SubjectNo)
diff <- diff_data(a, b)
render_diff(diff)

c<-as.data.frame(group2$SubjectNo)
d<-as.data.frame(DM$SubjectNo)
diff2 <- diff_data(c, d)
render_diff(diff2)

#CHN001-100091 is not included in feeding grouping
#because data of formula feeding is missing
#CHN001-100061分组有问题，因为3月龄写的混合喂养，
#但是没有写奶粉的量和次数,确认是混合喂养，但母亲未提供奶粉喂养信息（父母比较糊涂）

table(fas1$feeding_type_2)

table(DM$feeding_type_2)



x<- summary3 %>% filter(time == 0) %>%
  select(SEX,weight,height,bmi,
                     medical_history,feeding_type_2) %>%
  tbl_summary(
    by = feeding_type_2,
    # split table by group
    missing = "no",
    list(SEX ~ "Gender", weight~ "Weight(kg)",
         height ~ "Height(cm)",
         bmi ~ "BMI(kg/m2)",
         medical_history ~ "Medical history"),
    type = all_continuous() ~ "continuous2",
    statistic =all_continuous() ~ c("{median} ({p25}, {p75})","{mean} ({sd})", "{min}, {max}")# don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()

as_gt(x) %>% gt::gtsave("C:/Users/zhaiqiangrong/Desktop/tex.rtf",expand =10)


y<- summary3 %>% filter(GROUPING == " Group 2" & time == "180") %>%
  select(SEX,weight,height,bmi,medical_history) %>%
  tbl_summary(
    by = NULL,
    # split table by group
    missing = "no",
    list(SEX ~ "Gender", weight~ "Weight(kg)",
         height ~ "Height(cm)",
         bmi ~ "BMI(kg/m2)",
         medical_history ~ "Medical history"),
    type = all_continuous() ~ "continuous2",
    statistic =all_continuous() ~ c("{median} ({p25}, {p75})","{mean} ({sd})", "{min}, {max}")# don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()

as_gt(y) %>% gt::gtsave("C:/Users/zhaiqiangrong/Desktop/tex.rtf",expand =10)
