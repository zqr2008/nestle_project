library(gtsummary)
library(reshape2)
library(tidyverse)
library(PSweight)
library(lme4)
library(afex)
library(conflicted)
library(doBy)
library(ggResidpanel)

filter <- dplyr::filter

sos_1month <- summary2 %>% 
  select(time,c(3:6))

tbl_summary(
  sos_1month,
  by = time,
  # split table by group
  missing = "no",
  type = all_continuous() ~ "continuous2",
  statistic =all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}")# don't list missing data separately
) %>%
  add_n() %>% # add column with total number of non-missing observations # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()



tibia_z <- z_para %>% 
  select(time,c(41:42))

tbl_summary(
  tibia_z,
  by = time,
  # split table by group
  missing = "no",
  type = all_continuous() ~ "continuous2",
  statistic =all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}")# don't list missing data separately
) %>%
  add_n() %>% # add column with total number of non-missing observations # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()






GROUP_1_table1 <- summary1  %>%
  dplyr::filter(str_detect(GROUPING,"Group 1"))  %>%
  select(time,delivery_mode,weight,height,bmi,zwei,zlen,zwfl,zbmi) %>%
  mutate(time_delivery = paste(time,delivery_mode,sep = "-")) %>%
  select(-time,-delivery_mode)

GROUP_1_table2 <- summary1  %>%
  dplyr::filter(str_detect(GROUPING,"Group 1"))  %>% 
  filter(is.na(feeding_type_2)=="FALSE") %>%
  select(time,feeding_type_2,weight,height,bmi,zwei,zlen,zwfl,zbmi) %>%
  mutate(time_feeding = paste(time,feeding_type_2,sep = "-")) %>%
  select(-time,-feeding_type_2)

GROUP_2_table1 <- summary1  %>%
  dplyr::filter(str_detect(GROUPING,"Group 2"))  %>%
  select(time,delivery_mode,weight,height,bmi,zwei,zlen,zwfl,zbmi) %>%
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


as_gt(g1_table1) %>% gt::gtsave("C:/Users/zhaiqiangrong/Desktop/??????/interim/group1_by_delivery.png",expand =10)
as_gt(g1_table2) %>% gt::gtsave("C:/Users/zhaiqiangrong/Desktop/??????/interim/group1_by_feeding.png",expand =10)
as_gt(g2_table1) %>% gt::gtsave("C:/Users/zhaiqiangrong/Desktop/??????/interim/group2_by_delivery.png",expand =15)


propensity <- summary1 %>% 
  dplyr::filter(is.na(feeding_type_2)== "FALSE" & is.na(eth)== "FALSE")  %>%
  select(SubjectNo,Instance,weight,feeding_type_2,SEX,eth,GESTAGE,delivery_mode,education)

ps.mult <- as.factor(feeding_type_2) ~ as.factor(SEX) + as.factor(eth) + as.numeric(GESTAGE) +  as.factor(delivery_mode) + as.factor(education)
bal.mult <- SumStat(ps.formula = ps.mult, data = propensity, weight = c("IPW"))
weightss<-as.data.frame(bal.mult["ps.weights"])
propensity <- cbind(propensity,weightss) 

anthro_at_birth<-propensity %>%
  pivot_wider(names_from = "Instance",
              names_prefix = "weight",
              values_from = "weight") %>%
  dplyr::rename(weight_at_birth =`weightG1-V1 (birth + 10 days)`) %>%
  select(SubjectNo,weight_at_birth)

anthro_weight <- propensity %>%
  filter(str_detect(Instance,"birth") =="FALSE")

lmer_mode <- anthro_weight %>% left_join(anthro_at_birth,
                                         by="SubjectNo") %>%
  select(SubjectNo,weight,weight_at_birth,Instance,feeding_type_2,ps.weights.IPW) %>%
  drop_na(weight)

lmer_mode.mod <- lme4::lmer(weight~ weight_at_birth + Instance + 
                        feeding_type_2 + Instance*feeding_type_2 +
                        (1|SubjectNo), weights = ps.weights.IPW,
                      data=lmer_mode)

mixed(lmer_mode.mod, data = lmer_mode, weights = lmer_mode$ps.weights.IPW)
summary_weight <- summary(lmer_mode.mod)

resid_panel(lmer_mode.mod)
LSmeans(lmer_mode.mod)


