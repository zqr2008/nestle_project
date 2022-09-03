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
  newdata = y %>%  dplyr::mutate_at(.vars = vars(1:dim(y)[2]),.funs = 
                               function(x)ifelse(grepl("@",x),function_split(x),x))
  label(newdata) = label[1,]
  return(newdata)
}

edc <- "C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220831/2027NRC_FormExcel_3.0_20220831（以变量代码呈现）.xlsx"
edc_label <- "C:/Users/zhaiqiangrong/Desktop/雀巢/2027NRC_Data transfer_to BGI_20220831/2027NRC_FormExcel_3.0_20220831（以变量标签名呈现）.xlsx"
name_of <- excel_sheets(path = edc)
list_all <- lapply(name_of, function(x) read_excel(path = edc,
                                                   sheet = x))
list_all_label <- lapply(name_of, function(x) read_excel(path = edc_label, 
                                                         sheet = x,
                                                         col_names = F))
listA <- list()
for (i in (1:length(list_all))){ 
  print(name_of[i])
  listA <- append(listA,list(function_transfer(
                    as.data.frame(list_all[i]),
                    as.data.frame(list_all_label[i]))))
  }