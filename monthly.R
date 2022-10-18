library(reshape2)
library(reshape)
library(tidyverse)
library(readxl)
library(Hmisc)
library(xlsx)
library(plyr)
library(lubridate)
library(fuzzyjoin)
library(sqldf)
library(labelled)

data1 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨骼/1号机-1010.xlsx", 
                    sheet = "Sheet1")
data2 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨骼/3号机-1010.xlsx", 
                    sheet = "Sheet1")
data3 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨骼/5号机-1010.xlsx", 
                    sheet = "Sheet1")


id_sheet1 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨骼/1号机-1010.xlsx")
id_sheet2 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨骼/3号机-1010.xlsx",)
id_sheet3<- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨骼/5号机-1010.xlsx")


list1<-list(data1,data2,data3)
list2<-list(id_sheet1,id_sheet2,id_sheet3)
list3<-list("1号机器","3号机器","5号机器")


merge = as.data.frame(matrix(nrow=0,ncol=6))

for (x in (1:3)){
  #load each pair
  data=as.data.frame(list1[x]) 
  data <- data %>% mutate(machine_code=list3[x])
  id_sheet=as.data.frame(list2[x])
  #convert id into character
  id_sheet <- id_sheet %>%
    dplyr::mutate_at(.vars =vars(1), .fun=as.character)
  #handle colname and change detailed content to be consistent
  names(data)[1]<-"ID"
  #mutate vars'names and types to be consistent
  data <- data %>% 
    dplyr::mutate_at(.vars =vars(1,2), .fun=as.character) %>%
    mutate(SiteName=case_when(str_detect(SiteName,"RADIUS")~"桡骨 (左)",
                              str_detect(SiteName,"TIBIA")~"胫骨 (左)"))
  #merge id for each pair
  df <- data %>%
    left_join(id_sheet,by = "ID") %>% 
    select(PatientId,ResultDate,SiteName,VelocityMax,VelocityAverage,VelocityMin,machine_code) %>%
    filter(str_detect(PatientId,"test|HXRT") =="FALSE") %>%
    dplyr::mutate_at(.vars = vars(1),.funs = as.character)
  names(df)[3] <- "Site_bone"
  #add each one into the merge
  merge <- rbind(merge,df)
}

merge$ResultDate<-ymd(merge$ResultDate)


merge<-merge %>% filter( year(ResultDate)==2022 &  month(ResultDate) == 9) %>%
  mutate(是否做了骨密度 = "9月已做" ) %>%
  dplyr:: rename(研究编号=PatientId) 


base<-read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/肠骨孩子采集信息比对——给翟博(1).xlsx")

base2 <- base %>% left_join(merge,by="研究编号")
write.table(base2,file="C:/Users/zhaiqiangrong/Desktop/雀巢/base2.csv",sep=",",fileEncoding="GBK",row.names = F)
