library(reshape2)
library(reshape)
library(tidyverse)
library(readxl)
library(Hmisc)
library(xlsx)
library(plyr)
library(lubridate)
library(fuzzyjoin)
library(ggstatsplot)
library(wesanderson)
library(ggthemes)
library(patchwork)


windowsFonts(TNM = windowsFont("Times New Roman"))

#loading data of edc-sos
sos <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
                  sheet = "MO_SOS")
fa<-read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220729/2027NRC_FormExcel_2.0_20220729.xlsx", 
               sheet = "FA")
#during data preparation, the machine generate 5 seperate sheet of data, the id was separated from the data
#this part is for loading the sos data itself
data1 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/1/data1.xlsx")
data2 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220331/2/data2.xlsx")
data3 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/3/data3.xlsx")
data4 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/4/data4.xlsx")
data5 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/5/data5.xlsx")

#loadign id information sheet for each machine data sheet
id_sheet1 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/1/id_sheet_1.xlsx")
id_sheet2 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220331/2/id_sheet2.xlsx")
id_sheet3 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/3/id_sheet3.xlsx")
id_sheet4 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/4/id_sheet4.xlsx")
id_sheet5 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/骨密度仪电脑记录数据/20220804/5/id_sheet5.xlsx")

#handle colname to be consistent
#extract simple number of patient
sos$受试者编号<- substr(sos$受试者编号,start = 8,stop = 13)
fa$受试者编号<- substr(fa$受试者编号,start = 8,stop = 13)

#change name
names(fa)[3]<-"PatientId"
names(fa)[15]<-"SiteName"
names(sos)[3]<-"PatientId"
names(sos)[14]<-"ResultDate"
names(sos)[15]<-"SiteName"
#convert into numberic variable
sos$`结果(MOORRES)`<-as.numeric(sos$`结果(MOORRES)`)
#this step is important!! make sure the date is presented as year-month-1st format for fuzzy match
sos$ResultDate<-round_date(ymd(sos$ResultDate),'month')

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
  df<- data %>% left_join(id_sheet,by="ID") %>% 
    select(PatientId,ResultDate,SiteName,VelocityMax,VelocityAverage,VelocityMin,ZScore) %>%
    filter(str_detect(PatientId,"test|HXRT")=="FALSE") %>%
    dplyr::mutate_at(.vars = vars(1),.funs = as.character)
  
  #add each one into the merge
  merge<-rbind(merge,df)
  #export
  write.table(merge,file="C:/Users/zhaiqiangrong/Desktop/雀巢/merge.csv",sep=",",fileEncoding="GBK",row.names = F)
}

#this step is important!! make sure the date is presented as year-month-1st format for fuzzy match
merge$ResultDate<-round_date(ymd(merge$ResultDate),'month')

#merge by three keys and select inconsistency 
merge1<- sos %>% left_join(merge,by=c("PatientId","ResultDate","SiteName")) %>%
  select(PatientId,数据节,ResultDate,SiteName,VelocityMax,`结果(MOORRES)`,
         VelocityAverage,VelocityMin,ZScore) %>%
  filter(VelocityMax!=`结果(MOORRES)`)

#merge2 is final output of merging
merge2<- sos %>% left_join(merge,by=c("PatientId","ResultDate","SiteName")) %>%
  select(PatientId,ResultDate,SiteName,数据节,VelocityMax,`结果(MOORRES)`,
         VelocityAverage,VelocityMin,ZScore) %>% 
  group_by(PatientId,数据节,SiteName) %>% dplyr::mutate(n=n()) %>%  #handle mutiple tests
  filter(n<=1 | n>1 & abs(VelocityMax-`结果(MOORRES)`)<10)  %>% ungroup() #set rules that only range of 10 is tolerated 

merge3<-merge2 %>% left_join(fa,by=c("PatientId","数据节","SiteName")) 
merge3$`结果(FAORRES)`<-as.numeric(merge3$`结果(FAORRES)`)
merge3$数据节<-factor(merge3$数据节)
#merge3 <-merge3[complete.cases(merge3[,23]),]

pic1<-ggbetweenstats(
  data = merge3,  # 数据集 
  x = SiteName, # 分组变量 
  y = `结果(FAORRES)`,  # 目标变量 
  type = 'p', # p是参数检验，np是非参数检验 
  plot.type = 'boxviolin', # 还可以是，violin小提琴图，boxviolin二者叠加 
  xlab = '测量部位', 
  ylab = '长度', 
  pairwise.comparisons = T, #如果有多组的话，T可以直接显示组间比较的结果 
  pairwise.display = 'all', #显示组间比较有差异的，ns是显示没有差异的 
  p.adjust.method = 'bonferroni', #组间两两比较的p值校正的方法，如bonferroni 
  ggtheme = ggthemes::theme_fivethirtyeight(),
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # package from which color palette is to be taken
  palette = "Darjeeling1") # choosing a different color palette

pic1<-pic1 + theme(axis.title.x = element_text(family = "TNM", face = "bold", size = 18),
                   axis.title.y = element_text(family = "TNM", face = "bold", size = 18))

pic2<-ggbetweenstats(
  data = merge3,  # 数据集 
  x = SiteName, # 分组变量 
  y = VelocityMax,  # 目标变量 
  type = 'p', # p是参数检验，np是非参数检验 
  plot.type = 'boxviolin', # 还可以是，violin小提琴图，boxviolin二者叠加 
  xlab = '测量部位', 
  ylab = '长度', 
  pairwise.comparisons = T, #如果有多组的话，T可以直接显示组间比较的结果 
  pairwise.display = 'all', #显示组间比较有差异的，ns是显示没有差异的 
  p.adjust.method = 'bonferroni', #组间两两比较的p值校正的方法，如bonferroni 
  ggtheme = ggthemes::theme_fivethirtyeight(),
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # package from which color palette is to be taken
  palette = "Royal1") # choosing a different color palette

pic2<-pic2 + theme(axis.title.x = element_text(family = "TNM", face = "bold", size = 18),
                   axis.title.y = element_text(family = "TNM", face = "bold", size = 18))

pic3<-ggbetweenstats(
  data = merge3,  # 数据集 
  x = SiteName, # 分组变量 
  y = VelocityAverage,  # 目标变量 
  type = 'p', # p是参数检验，np是非参数检验 
  plot.type = 'boxviolin', # 还可以是，violin小提琴图，boxviolin二者叠加 
  xlab = '测量部位', 
  ylab = '长度', 
  pairwise.comparisons = T, #如果有多组的话，T可以直接显示组间比较的结果 
  pairwise.display = 'all', #显示组间比较有差异的，ns是显示没有差异的 
  p.adjust.method = 'bonferroni', #组间两两比较的p值校正的方法，如bonferroni 
  ggtheme = ggthemes::theme_fivethirtyeight(),
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # package from which color palette is to be taken
  palette = "Royal2") # choosing a different color palette
pic3<-pic3 + theme(axis.title.x = element_text(family = "TNM", face = "bold", size = 18),
                   axis.title.y = element_text(family = "TNM", face = "bold", size = 18))


pic4<-ggbetweenstats(
  data = merge3,  # 数据集 
  x = SiteName, # 分组变量 
  y = VelocityMin,  # 目标变量 
  type = 'p', # p是参数检验，np是非参数检验 
  plot.type = 'boxviolin', # 还可以是，violin小提琴图，boxviolin二者叠加 
  xlab = '测量部位', 
  ylab = '长度', 
  pairwise.comparisons = T, #如果有多组的话，T可以直接显示组间比较的结果 
  pairwise.display = 'all', #显示组间比较有差异的，ns是显示没有差异的 
  p.adjust.method = 'bonferroni', #组间两两比较的p值校正的方法，如bonferroni 
  ggtheme = ggthemes::theme_fivethirtyeight(),
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # package from which color palette is to be taken
  palette = "Darjeeling2") 
# choosing a different color palette
pic4<-pic4 + theme(axis.title.x = element_text(family = "TNM", face = "bold", size = 18),
                   axis.title.y = element_text(family = "TNM", face = "bold", size = 18))



ggbetweenstats(
  data = merge3,  # 数据集 
  x = 数据节, # 分组变量 
  y = VelocityMax,  # 目标变量 
  type = 'p', # p是参数检验，np是非参数检验 
  plot.type = 'boxviolin', # 还可以是，violin小提琴图，boxviolin二者叠加 
  xlab = '测量部位', 
  ylab = '长度', 
  pairwise.comparisons = F, #如果有多组的话，T可以直接显示组间比较的结果 
  pairwise.display = 'all', #显示组间比较有差异的，ns是显示没有差异的 
  p.adjust.method = 'bonferroni', #组间两两比较的p值校正的方法，如bonferroni 
  ggtheme = ggthemes::theme_fivethirtyeight(),
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # package from which color palette is to be taken
  palette = "Darjeeling1") # choosing a different color palette


ggbetweenstats(
  data = merge3,  # 数据集 
  x = 数据节, # 分组变量 
  y = VelocityMax,  # 目标变量 
  type = 'p', # p是参数检验，np是非参数检验 
  plot.type = 'boxviolin', # 还可以是，violin小提琴图，boxviolin二者叠加 
  xlab = '测量部位', 
  ylab = '长度', 
  pairwise.comparisons = F, #如果有多组的话，T可以直接显示组间比较的结果 
  pairwise.display = 'all', #显示组间比较有差异的，ns是显示没有差异的 
  p.adjust.method = 'bonferroni', #组间两两比较的p值校正的方法，如bonferroni 
  ggtheme = ggthemes::theme_fivethirtyeight(),
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # package from which color palette is to be taken
  palette = "Darjeeling1") # choosing a different color palette

ggbetweenstats(
  data = merge3,  # 数据集 
  x = 数据节, # 分组变量 
  y = VelocityAverage,  # 目标变量 
  type = 'p', # p是参数检验，np是非参数检验 
  plot.type = 'boxviolin', # 还可以是，violin小提琴图，boxviolin二者叠加 
  xlab = '测量部位', 
  ylab = '长度', 
  pairwise.comparisons = F, #如果有多组的话，T可以直接显示组间比较的结果 
  pairwise.display = 'all', #显示组间比较有差异的，ns是显示没有差异的 
  p.adjust.method = 'bonferroni', #组间两两比较的p值校正的方法，如bonferroni 
  ggtheme = ggthemes::theme_fivethirtyeight(),
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # package from which color palette is to be taken
  palette = "Darjeeling1") # choosing a different color palette





ggbetweenstats(
  data = merge3,  # 数据集 
  x = 数据节, # 分组变量 
  y = VelocityMin,  # 目标变量 
  type = 'p', # p是参数检验，np是非参数检验 
  plot.type = 'boxviolin', # 还可以是，violin小提琴图，boxviolin二者叠加 
  xlab = '测量部位', 
  ylab = '长度', 
  pairwise.comparisons = F, #如果有多组的话，T可以直接显示组间比较的结果 
  pairwise.display = 'all', #显示组间比较有差异的，ns是显示没有差异的 
  p.adjust.method = 'bonferroni', #组间两两比较的p值校正的方法，如bonferroni 
  ggtheme = ggthemes::theme_fivethirtyeight(),
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # package from which color palette is to be taken
  palette = "Darjeeling1") # choosing a different color palette





#filter(is.na(`结果(MOORRES)`)==FALSE & is.na(VelocityMax)) 
#output
write.table(missing,file="C:/Users/zhaiqiangrong/Desktop/雀巢/merge2.csv",sep=",",fileEncoding="GBK",row.names = F)
