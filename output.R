library(tableone)

GROUP_1_table1<- summary1 %>% dplyr::filter(time == 0) %>%
  dplyr::filter(str_detect(GROUPING,"Group 1")) 

varss=c("体重","身高","bmi","zwei","zlen","zwfl","zbmi") 

table1<-CreateTableOne(vars = varss,data = GROUP_1_table1,strata = "delivery_mode")
table_mat <- print(table1, quote = F, noSpaces = , printToggle = FALSE)
write.table(table_mat,file="C:/Users/zhaiqiangrong/Desktop/雀巢/interim/anth_table_group1_visit1.csv", sep=",",fileEncoding="GBK",row.names = T)

summary(table1)

GROUP_1_table2<- summary1 %>% dplyr::filter(time == 30) %>%
  dplyr::filter(str_detect(GROUPING,"Group 1")) 
varss=c("体重","身高","bmi","zwei","zlen","zwfl","zbmi") 
table2<-CreateTableOne(vars = varss,data = GROUP_1_table2,strata = "delivery_mode")
table_mat <- print(table2, quote = F, noSpaces = , printToggle = FALSE)
write.table(table_mat,file="C:/Users/zhaiqiangrong/Desktop/雀巢/interim/anth_table_group1_visit2.csv", sep=",",fileEncoding="GBK",row.names = T)

summary(table2)



GROUP_1_table3<- summary1 %>% dplyr::filter(time == 90) %>%
  dplyr::filter(str_detect(GROUPING,"Group 1")) 
varss=c("体重","身高","bmi","zwei","zlen","zwfl","zbmi") 
table3<-CreateTableOne(vars = varss,data = GROUP_1_table3,strata = "delivery_mode")
table_mat <- print(table3, quote = F, noSpaces = , printToggle = FALSE)
write.table(table_mat,file="C:/Users/zhaiqiangrong/Desktop/雀巢/interim/anth_table_group1_visit3.csv", sep=",",fileEncoding="GBK",row.names = T)

summary(table3)


GROUP_1_table4<- summary1 %>% dplyr::filter(time == 120) %>%
  dplyr::filter(str_detect(GROUPING,"Group 1")) 
varss=c("体重","身高","bmi","zwei","zlen","zwfl","zbmi") 
table4<-CreateTableOne(vars = varss,data = GROUP_1_table4,strata = "delivery_mode")
table_mat <- print(table4, quote = F, noSpaces = , printToggle = FALSE)
write.table(table_mat,file="C:/Users/zhaiqiangrong/Desktop/雀巢/interim/anth_table_group1_visit4.csv", sep=",",fileEncoding="GBK",row.names = T)

summary(table4)


GROUP_1_table5<- summary1 %>% dplyr::filter(time == 180) %>%
  dplyr::filter(str_detect(GROUPING,"Group 1")) 
varss=c("体重","身高","bmi","zwei","zlen","zwfl","zbmi") 
table5<-CreateTableOne(vars = varss,data = GROUP_1_table5,strata = "delivery_mode")
table_mat <- print(table5, quote = F, noSpaces = , printToggle = FALSE)
write.table(table_mat,file="C:/Users/zhaiqiangrong/Desktop/雀巢/interim/anth_table_group1_visit5.csv", sep=",",fileEncoding="GBK",row.names = T)

summary(table5)


GROUP_2_table1<- summary1 %>% dplyr::filter(time == 180) %>%
  dplyr::filter(str_detect(GROUPING,"Group 2")) 
varss=c("体重","身高","bmi","zwei","zlen","zwfl","zbmi") 
table6<-CreateTableOne(vars = varss,data = GROUP_2_table1,strata = "delivery_mode")
table_mat <- print(table6, quote = F, noSpaces = , printToggle = FALSE)
write.table(table_mat,file="C:/Users/zhaiqiangrong/Desktop/雀巢/interim/anth_table_group2_visit1.csv", sep=",",fileEncoding="GBK",row.names = T)

summary(table6)


GROUP_2_table2<- summary1 %>% dplyr::filter(time == 270) %>%
  dplyr::filter(str_detect(GROUPING,"Group 2")) 
varss=c("体重","身高","bmi","zwei","zlen","zwfl","zbmi") 
table7<-CreateTableOne(vars = varss,data = GROUP_2_table2,strata = "delivery_mode")
table_mat <- print(table7, quote = F, noSpaces = , printToggle = FALSE)
write.table(table_mat,file="C:/Users/zhaiqiangrong/Desktop/雀巢/interim/anth_table_group2_visit2.csv", sep=",",fileEncoding="GBK",row.names = T)

summary(table7)




GROUP_2_table3<- summary1 %>% dplyr::filter(time == 360) %>%
  dplyr::filter(str_detect(GROUPING,"Group 2")) 
varss=c("体重","身高","bmi","zwei","zlen","zwfl","zbmi") 
table8<-CreateTableOne(vars = varss,data = GROUP_2_table3,strata = "delivery_mode")
table_mat <- print(table8, quote = F, noSpaces = , printToggle = FALSE)
write.table(table_mat,file="C:/Users/zhaiqiangrong/Desktop/雀巢/interim/anth_table_group2_visit3.csv", sep=",",fileEncoding="GBK",row.names = T)

summary(table8)





varss=c("体重","身高","bmi","zwei","zlen","zwfl","zbmi") 
big<-CreateTableOne(vars = varss,data = summary1,strata = c("delivery_mode","time"),test = TRUE)
table_mat <- print(big, quote = F, noSpaces = , printToggle = FALSE)

write.table(table_mat,file="C:/Users/zhaiqiangrong/Desktop/雀巢/interim/anth_table1.csv", sep=",",fileEncoding="GBK",row.names = T)

varss=c("体重","身高","bmi","zwei","zlen","zwfl","zbmi") 
big2<-CreateTableOne(vars = varss,data = summary1,strata = c("feeding_type_2","time"),test = TRUE)
table_mat <- print(big2, quote = F, noSpaces = , printToggle = FALSE)

write.table(table_mat,file="C:/Users/zhaiqiangrong/Desktop/雀巢/interim/anth_table2.csv", sep=",",fileEncoding="GBK",row.names = T)


