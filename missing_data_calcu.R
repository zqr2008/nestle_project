library(tableone)
library(readxl)
library(tidyverse)
library(conflicted)
library(daff)


LB3 <- as.data.frame(listB[["LB_SAMP3"]])
sos <- as.data.frame(listB[["MO_SOS"]])
LB_SAMP2 <- as.data.frame(listB[["LB_SAMP2"]])
LB_RES <- as.data.frame(listB[["LB_RES"]])
QS9 <- as.data.frame(listB[["QS9"]])
QS_FOOD <- as.data.frame(listB[["QS_FOOD"]])
#bone
missing <- sos %>%
  dplyr::filter(str_detect(Instance,"G1-V6")=="FALSE") %>%  
  left_join(group2,by= "SubjectNo") %>%
  mutate(Present = case_when(is.na(as.numeric(MOORRES))==TRUE ~"No",
                             TRUE ~ "Yes")) %>%
  mutate_at(.vars = vars(33),.fun = function(x)ifelse(is.na(x),"group2",x)) %>%
  mutate(grouping = case_when(str_detect(SubjectNo,"CHN001-100")~ "Group 1",
                              str_detect(SubjectNo,"CHN001-200")~ "Group 2"))


missing_t <- as.data.frame(table(missing$feeding_type_2,missing$Present))
missing_t <- missing_t %>% 
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Feeding_group = Var1,
         Present  = Var2) %>%
  arrange(Feeding_group) %>%
  filter(Present == "No")


by_group <- as.data.frame(table(missing$grouping,missing$Present))
by_group <- by_group %>%
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Group = Var1,
         Present  = Var2) %>%
  arrange(Group) %>%
  filter(Present == "No")
  

x <- table(missing$Present,missing$Instance.x)
x <- as.data.frame(x)
x <- x %>% group_by(Var2) %>% mutate(percentage = 100*round(Freq/sum(Freq),3) ) %>%
  rename(Present = Var1,
         Visit   = Var2) %>%
  filter(Present == "No")

write.table(x,file="C:/Users/zhaiqiangrong/Desktop/stool_missing.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(missing_t,file="C:/Users/zhaiqiangrong/Desktop/stool_missing_t.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(by_group,file="C:/Users/zhaiqiangrong/Desktop/by_group.csv",sep=",",fileEncoding="GBK",row.names = F)


# fecal
missing <- LB3 %>% 
  dplyr::filter(str_detect(Instance,"G1-V6")=="FALSE")  %>%
  left_join(group2,by= "SubjectNo") %>%
  mutate_at(.vars = vars(34), .fun = function(x)ifelse(is.na(x),"group2",x))%>%
  mutate(grouping = case_when(str_detect(SubjectNo,"CHN001-100")~ "Group 1",
                              str_detect(SubjectNo,"CHN001-200")~ "Group 2"))


missing_t <- as.data.frame(table(missing$feeding_type_2,missing$LB3PERF))
missing_t <- missing_t %>% 
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Feeding_group = Var1,
         Present  = Var2) %>%
  arrange(Feeding_group) %>%
  filter(Present == " No")

x <- table(missing$LB3PERF,missing$Instance.x)
x <- as.data.frame(x)
x <- x %>% group_by(Var2) %>% mutate(percentage = 100*round(Freq/sum(Freq),3) ) %>%
  rename(Present = Var1,
         Visit   = Var2) %>%
  filter(Present == " No")


by_group <- as.data.frame(table(missing$grouping,missing$LB3PERF))
by_group <- by_group %>%
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Group = Var1,
         Present  = Var2) %>%
  arrange(Group) %>%
  filter(Present == " No")



write.table(x,file="C:/Users/zhaiqiangrong/Desktop/stool_missing.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(missing_t,file="C:/Users/zhaiqiangrong/Desktop/stool_missing_t.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(by_group,file="C:/Users/zhaiqiangrong/Desktop/by_group.csv",sep=",",fileEncoding="GBK",row.names = F)



# urine
missing <- LB_SAMP2 %>%  
  dplyr::filter(str_detect(Instance,"G1-V6")=="FALSE")  %>% 
  left_join(group2,by= "SubjectNo") %>%
  mutate_at(.vars = vars(32), .fun = function(x)ifelse(is.na(x),"group2",x)) %>%
  mutate(grouping = case_when(str_detect(SubjectNo,"CHN001-100")~ "Group 1",
                              str_detect(SubjectNo,"CHN001-200")~ "Group 2"))




missing_t <- as.data.frame(table(missing$feeding_type_2,missing$LB2PERF))
missing_t <- missing_t %>% 
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Feeding_group = Var1,
         Present  = Var2) %>%
  arrange(Feeding_group) %>%
  filter(Present == " No")


by_group <- as.data.frame(table(missing$grouping,missing$LB2PERF))
by_group <- by_group %>%
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Group = Var1,
         Present  = Var2) %>%
  arrange(Group) %>%
  filter(Present == " No")

x <- table(missing$LB2PERF,missing$Instance.x)
x <- as.data.frame(x)
x <- x %>% group_by(Var2) %>% mutate(percentage = 100*round(Freq/sum(Freq),3) ) %>%
  rename(Present = Var1,
         Visit   = Var2) %>%
  filter(Present == " No")

write.table(x,file="C:/Users/zhaiqiangrong/Desktop/stool_missing.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(missing_t,file="C:/Users/zhaiqiangrong/Desktop/stool_missing_t.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(by_group,file="C:/Users/zhaiqiangrong/Desktop/by_group.csv",sep=",",fileEncoding="GBK",row.names = F)



# milk
missing <- LB_RES %>% 
  dplyr::filter(str_detect(Instance,"G1-V6")=="FALSE") %>%
  left_join(group2,by= "SubjectNo") %>%
  mutate_at(.vars = vars(39), .fun = function(x)ifelse(is.na(x),"group2",x)) %>%
  mutate(grouping = case_when(str_detect(SubjectNo,"CHN001-100")~ "Group 1",
                              str_detect(SubjectNo,"CHN001-200")~ "Group 2"))


missing_t <- as.data.frame(table(missing$feeding_type_2,missing$LBPERF))
missing_t <- missing_t %>% 
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Feeding_group = Var1,
         Present  = Var2) %>%
  arrange(Feeding_group) %>%
  filter(Present == " No")


by_group <- as.data.frame(table(missing$grouping,missing$LBPERF))
by_group <- by_group %>%
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Group = Var1,
         Present  = Var2) %>%
  arrange(Group) %>%
  filter(Present == " No")



x <- table(missing$LBPERF,missing$Instance.x)
x <- as.data.frame(x)
x <- x %>% group_by(Var2) %>% mutate(percentage = 100*round(Freq/sum(Freq),3) ) %>%
  rename(Present = Var1,
         Visit   = Var2) %>%
  filter(Present == " No")

write.table(x,file="C:/Users/zhaiqiangrong/Desktop/stool_missing.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(missing_t,file="C:/Users/zhaiqiangrong/Desktop/stool_missing_t.csv",sep=",",fileEncoding="GBK",row.names = F)



#tibia
missing <- sos %>%
  dplyr::filter(str_detect(Instance,"G1-V6")=="FALSE") %>%  
  filter(str_detect(MOLOC,"胫骨")) %>%
  left_join(group2,by= "SubjectNo") %>%
  mutate(Present = case_when(is.na(as.numeric(MOORRES))==TRUE ~"No",
                             TRUE ~ "Yes")) %>%
  mutate_at(.vars = vars(33),.fun = function(x)ifelse(is.na(x),"group2",x)) %>%
  mutate(grouping = case_when(str_detect(SubjectNo,"CHN001-100")~ "Group 1",
                              str_detect(SubjectNo,"CHN001-200")~ "Group 2"))


missing_t <- as.data.frame(table(missing$feeding_type_2,missing$Present))
missing_t <- missing_t %>% 
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Feeding_group = Var1,
         Present  = Var2) %>%
  arrange(Feeding_group) %>%
  filter(Present == "No")


by_group <- as.data.frame(table(missing$grouping,missing$Present))
by_group <- by_group %>%
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Group = Var1,
         Present  = Var2) %>%
  arrange(Group) %>%
  filter(Present == "No")



x <- table(missing$Present,missing$Instance.x)
x <- as.data.frame(x)
x <- x %>% group_by(Var2) %>% mutate(percentage = 100*round(Freq/sum(Freq),3) ) %>%
  rename(Present = Var1,
         Visit   = Var2) %>%
  filter(Present == "No")

write.table(x,file="C:/Users/zhaiqiangrong/Desktop/stool_missing.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(missing_t,file="C:/Users/zhaiqiangrong/Desktop/stool_missing_t.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(by_group,file="C:/Users/zhaiqiangrong/Desktop/by_group.csv",sep=",",fileEncoding="GBK",row.names = F)


#questionnaire 
missing <- QS9 %>% 
  dplyr::filter(str_detect(Instance,"G1-V6")=="FALSE") %>%
  left_join(group2,by= "SubjectNo") %>%
  mutate_at(.vars = vars(29), .fun = function(x)ifelse(is.na(x),"group2",x))  %>%
  mutate(grouping = case_when(str_detect(SubjectNo,"CHN001-100")~ "Group 1",
                              str_detect(SubjectNo,"CHN001-200")~ "Group 2"))



missing_t <- as.data.frame(table(missing$feeding_type_2,missing$QS9PERF))
missing_t <- missing_t %>% 
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Feeding_group = Var1,
         Present  = Var2) %>%
  arrange(Feeding_group) %>%
  filter(Present == " No")


x <- table(missing$QS9PERF,missing$Instance.x)
x <- as.data.frame(x)
x <- x %>% group_by(Var2) %>% mutate(percentage = 100*round(Freq/sum(Freq),3) ) %>%
  rename(Present = Var1,
         Visit   = Var2) %>%
  filter(Present == " No")



by_group <- as.data.frame(table(missing$grouping,missing$QS9PERF))
by_group <- by_group %>%
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Group = Var1,
         Present  = Var2) %>%
  arrange(Group) %>%
  filter(Present == " No")



write.table(x,file="C:/Users/zhaiqiangrong/Desktop/stool_missing.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(missing_t,file="C:/Users/zhaiqiangrong/Desktop/stool_missing_t.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(by_group,file="C:/Users/zhaiqiangrong/Desktop/by_group.csv",sep=",",fileEncoding="GBK",row.names = F)


#food 
missing <- QS_FOOD %>% 
  dplyr::filter(str_detect(Instance,"G1-V6")=="FALSE") %>%
  left_join(group2,by= "SubjectNo") %>%
  mutate_at(.vars = vars(32), .fun = function(x)ifelse(is.na(x),"group2",x))  %>%
  mutate(grouping = case_when(str_detect(SubjectNo,"CHN001-100")~ "Group 1",
                              str_detect(SubjectNo,"CHN001-200")~ "Group 2"))



missing_t <- as.data.frame(table(missing$feeding_type_2,missing$FDPEFR1))
missing_t <- missing_t %>% 
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Feeding_group = Var1,
         Present  = Var2) %>%
  arrange(Feeding_group) %>%
  filter(Present == " No")


x <- table(missing$FDPEFR1,missing$Instance.x)
x <- as.data.frame(x)
x <- x %>% group_by(Var2) %>% mutate(percentage = 100*round(Freq/sum(Freq),3) ) %>%
  rename(Present = Var1,
         Visit   = Var2) %>%
  filter(Present == " No")

by_group <- as.data.frame(table(missing$grouping,missing$FDPEFR1))
by_group <- by_group %>%
  group_by(Var1) %>% 
  mutate(percentage = 100*round(Freq/sum(Freq),3)) %>%
  rename(Group = Var1,
         Present  = Var2) %>%
  arrange(Group) %>%
  filter(Present == " No")

write.table(x,file="C:/Users/zhaiqiangrong/Desktop/stool_missing.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(missing_t,file="C:/Users/zhaiqiangrong/Desktop/stool_missing_t.csv",sep=",",fileEncoding="GBK",row.names = F)
write.table(by_group,file="C:/Users/zhaiqiangrong/Desktop/by_group.csv",sep=",",fileEncoding="GBK",row.names = F)
