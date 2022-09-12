library(gtsummary)
library(reshape2)
library(tidyverse)
library(PSweight)

GROUP_1_table1 <- summary1  %>%
  dplyr::filter(str_detect(GROUPING,"Group 1"))  %>%
  select(time,delivery_mode,体重,身高,bmi,zwei,zlen,zwfl,zbmi) %>%
  mutate(time_delivery = paste(time,delivery_mode,sep = "-")) %>%
  select(-time,-delivery_mode)

GROUP_1_table2 <- summary1  %>%
  dplyr::filter(str_detect(GROUPING,"Group 1"))  %>% 
  filter(is.na(feeding_type_2)=="FALSE") %>%
  select(time,feeding_type_2,体重,身高,bmi,zwei,zlen,zwfl,zbmi) %>%
  mutate(time_feeding = paste(time,feeding_type_2,sep = "-")) %>%
  select(-time,-feeding_type_2)

GROUP_2_table1 <- summary1  %>%
  dplyr::filter(str_detect(GROUPING,"Group 2"))  %>%
  select(time,delivery_mode,体重,身高,bmi,zwei,zlen,zwfl,zbmi) %>%
  mutate(time_delivery = paste(time,delivery_mode,sep = "-")) %>%
  select(-time,-delivery_mode)
  
g1_table1 <- tbl_summary(
    GROUP_1_table1,
    by = time_delivery,
    # split table by group
    missing = "no",
    type = all_continuous() ~ "continuous2",
    statistic =all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}")# don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()


g1_table2 <- tbl_summary(
  GROUP_1_table2,
  by = time_feeding,
  # split table by group
  missing = "no",
  type = all_continuous() ~ "continuous2",
  statistic =all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}")# don't list missing data separately
) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()
  

g2_table1 <- tbl_summary(
  GROUP_2_table1,
  by = time_delivery,
  # split table by group
  missing = "no",
  type = all_continuous() ~ "continuous2",
  statistic =all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}")# don't list missing data separately
) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()


as_gt(g1_table1) %>% gt::gtsave("C:/Users/zhaiqiangrong/Desktop/雀巢/interim/group1_by_delivery.png",expand =10)
as_gt(g1_table2) %>% gt::gtsave("C:/Users/zhaiqiangrong/Desktop/雀巢/interim/group1_by_feeding.png",expand =10)
as_gt(g2_table1) %>% gt::gtsave("C:/Users/zhaiqiangrong/Desktop/雀巢/interim/group2_by_delivery.png",expand =15)



ancova <- summary1 %>%
  dplyr::filter(str_detect(GROUPING,"Group 1") & time == 0)   %>% 
  filter(is.na(feeding_type_2)=="FALSE")  %>%
  select(feeding_type_2,体重,身高,bmi,zwei,zlen,zwfl,zbmi) 

propensity <- summary1 %>%
  select(feeding_type_2,SEX,eth,GESTAGE,delivery_mode,education) %>%
  filter(is.na(feeding_type_2)== "FALSE" & is.na(eth)== "FALSE") 

ps.mult <- as.factor(feeding_type_2) ~ as.factor(SEX) + as.factor(eth) + as.numeric(GESTAGE) +  as.factor(delivery_mode) + as.factor(education)
bal.mult <- SumStat(ps.formula = ps.mult, data = propensity, weight = c("IPW"))
