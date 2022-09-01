library(Matching)
library(survey)
library(reshape2)
library(tidyverse)
library(readxl)
library(Hmisc)
library(xlsx)

#提取@之前的function
function_split <- function(x){
  holds = str_split(x, "@")
  hold = holds[[1]][2]
  return(hold)}
#提取，加上label
function_transfer <- function(y,label){
  newdata = y %>%  mutate_at(.vars = vars(1:dim(y)[2]),.funs = 
                               function(x)ifelse(grepl("@",x),function_split(x),x))
  label(newdata) = label[1,]
  return(newdata)
}

#导入数据，每个sheet都有2张表，一张是以变量代码呈现，一张是以变量标签名呈现
QS11 <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220831/2027NRC_FormExcel_3.0_20220831（以变量代码呈现）.xlsx", 
                   sheet = "QS11")
QS11_label <- read_excel("C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220831/2027NRC_FormExcel_3.0_20220831（以变量标签名呈现）.xlsx", 
                         sheet = "QS11",col_names = F)

#第一个输入文件QS11是一张是以变量代码呈现的sheet,
#第二个输入文件QS11_label是以变量标签名呈现的sheet
#一定要保证输入的顺序，同时两个文件是严格对应sheet
#test即为加了label同时删除@的前的中文的清洗文件
test<- function_transfer(QS11,QS11_label)