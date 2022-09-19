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
#loading data of edc-sos
sos <- as.data.frame(listB[["MO_SOS"]])

#during data preparation, the machine generate 5 seperate sheet of data, the id was separated from the data
#this part is for loading the sos data itself
data1 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/1/data1.xlsx")
data2 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220331/2/data2.xlsx")
data3 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/3/data3.xlsx")
data4 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/4/data4.xlsx")
data5 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/5/data5.xlsx")
data5$ResultDate <- mdy(data5$ResultDate)

#loadign id information sheet for each machine data sheet
id_sheet1 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/1/id_sheet_1.xlsx")
id_sheet2 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220331/2/id_sheet2.xlsx")
id_sheet3 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/3/id_sheet3.xlsx")
id_sheet4 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/4/id_sheet4.xlsx")
id_sheet5 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/5/id_sheet5.xlsx")

#handle colname to be consistent
#extract simple number of patient
sos$SubjectNo <- substr(sos$SubjectNo,start = 8,stop = 13)
sos <- sos %>% 
  dplyr::rename(PatientId = SubjectNo,
                ResultDate = MODAT,
                Site_bone = MOLOC
                )  %>%
  mutate_at(.vars = vars(17),.funs = as.numeric) %>%
  mutate_at(.vars = vars(14),.funs = function(x)as.numeric(as.Date(x))) %>%
  filter(str_detect(Instance,"G1-V6")=="FALSE") 
  


#set loop list
#list1 is for data sheet
list1<-list(data1,data2,data3,data4,data5)
#list2 is for id sheet
list2<-list(id_sheet1,id_sheet2,id_sheet3,id_sheet4,id_sheet5)
#set up empty dataframe
merge = as.data.frame(matrix(nrow=0,ncol=6))
#handle colname and change type
for (x in (1:5)){
  #load each pair
  data=as.data.frame(list1[x])
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
    select(PatientId,ResultDate,SiteName,VelocityMax,VelocityAverage,VelocityMin,ZScore) %>%
    filter(str_detect(PatientId,"test|HXRT") =="FALSE") %>%
    dplyr::mutate_at(.vars = vars(1),.funs = as.character)
  names(df)[3] <- "Site_bone"
  #add each one into the merge
  merge <- rbind(merge,df)
}

#this step is important!! make sure the date is presented as year-month-1st format for fuzzy match
merge <- merge %>%
  mutate_at(.vars = vars(2),.funs = function(x)as.numeric(as.Date(x))) %>%
  mutate(upper_date = ResultDate+15) %>%
  mutate(lower_date = ResultDate-15)

merge_join <-sqldf("select sos.PatientId,sos.Site_bone,Instance,
       MOORRES,VelocityMax,VelocityAverage,VelocityMin,
       sos.ResultDate,upper_date,lower_date
       from sos left join merge on  
       sos.PatientId = merge.PatientId and 
       sos.Site_bone = merge.Site_bone and
       sos.ResultDate < merge.upper_date and
       sos.ResultDate > merge.lower_date",
               method = "raw")

merge_check_duplicate <- merge_join %>% 
  group_by(PatientId,Site_bone,Instance) %>%
  dplyr::mutate(n=n()) %>%
  filter(n>1) %>% 
  filter(MOORRES==VelocityMax)

merge_without_dup <- merge_join %>% 
  group_by(PatientId,Site_bone,Instance) %>%
  dplyr::mutate(n=n()) %>%
  filter(n==1)

merge_after<-rbind(merge_check_duplicate,merge_without_dup)

merge_check <- merge_after %>%
  filter(MOORRES != VelocityMax)

merge_check_miss <-  merge_after %>% 
  filter(is.na(MOORRES) == "FALSE"  & 
         is.na(VelocityMax) == "TRUE")

merge_check_miss_inverse<-  merge_after %>% 
  filter(is.na(MOORRES) == "TRUE"  & 
         is.na(VelocityMax) == "FALSE")

merge_for_analysis<-merge_after %>% select(PatientId,Site_bone,Instance,VelocityMax) 
merge_for_analysis <- reshape::cast(
  merge_for_analysis,PatientId+Instance ~Site_bone) 

fa <- as.data.frame(listB[["FA"]])
fa$SubjectNo  <- substr(fa$SubjectNo,start = 8,stop = 13)
fa <- fa %>% 
  dplyr::rename(PatientId = SubjectNo,
                Site_bone = FALOC) %>%
  select(PatientId,Instance,Site_bone,FAORRES)
fa <-reshape::cast(fa,PatientId + Instance~ Site_bone)
fa <- fa[,c(1,2,5,6)]
fa <-remove_labels(fa)  
merge_bone<- fa %>% left_join(merge_for_analysis,by=c("PatientId","Instance"),
                                              suffix = c("length","sos")) 
merge_bone$Instance<-trimws(merge_bone$Instance)
#merge3 <-merge3[complete.cases(merge3[,23]),]


#filter(is.na(`结果(MOORRES)`)==FALSE & is.na(VelocityMax)) 
#output
write.table(missing,file="C:/Users/zhaiqiangrong/Desktop/雀巢/merge2.csv",sep=",",fileEncoding="GBK",row.names = F)




