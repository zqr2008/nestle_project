library(gtsummary)
library(reshape2)
library(tidyverse)
library(PSweight)
library(lme4)
library(afex)
library(conflicted)
library(doBy)
library(ggResidpanel)
library(animation)


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


#propensity score
list_anth <- c("height","weight","bmi","zwei","zlen","zwfl","zbmi")

propensity <- summary1 %>% 
  dplyr::filter(is.na(feeding_type_2)== "FALSE")  %>%
  select(SubjectNo,Instance,list_anth,feeding_type_2,SEX,eth,GESTAGE,delivery_mode,child_number,education)

ps.mult <- as.factor(feeding_type_2) ~ as.factor(SEX) + as.factor(eth) + 
           as.numeric(GESTAGE) + as.factor(delivery_mode) + 
           as.numeric(child_number) + as.factor(education)

bal.mult <- SumStat(ps.formula = ps.mult, data = propensity, weight = c("IPW"))
plot(bal.mult, type = "density") 

#plot difference 
pdf("propensity.pdf",width=25,height=10)
plot(bal.mult, type = "balance", metric = "PSD",cex.axis=2.5)
dev.off() 

#combine into one
weightss <- as.data.frame(bal.mult["ps.weights"])
propensity_score <- cbind(propensity,weightss) 

anthro_other_time <- propensity_score %>%
  filter(str_detect(Instance,"birth") =="FALSE")


# mixed liner model building in loop for each variable

list_model <- list()
saveGIF(for (x in (1:7)){
  anthro_at_birth <- propensity_score %>% 
  select(SubjectNo,Instance,list_anth[x]) %>%
  pivot_wider(names_from = "Instance",
              names_prefix = list_anth[x],
              values_from = list_anth[x]) %>%
  dplyr::rename(!!paste0("at_birth",sep = "_",list_anth[x]) := 2) %>%
  select(1:2) 
  
  para_at_birth <- noquote(paste0("at_birth",sep = "_",list_anth[x]))
  anth <- noquote(list_anth[x])
  
  lmer_mode <- anthro_other_time %>% 
    left_join(anthro_at_birth, by = "SubjectNo")  %>%
    drop_na(list_anth[x])

  lmer_mode.mod <- lme4::lmer(get(anth)~ get(para_at_birth) + Instance + 
                        feeding_type_2 + Instance*feeding_type_2 +
                        (1|SubjectNo), weights = ps.weights.IPW,
                      data = lmer_mode)
  
  print(resid_panel(lmer_mode.mod,title.opt = TRUE))
})



afex::mixed(lmer_mode.mod, data = lmer_mode, weights = lmer_mode$ps.weights.IPW)

summary_weight <- summary(lmer_mode.mod)




resid_panel(lmer_mode.mod)
LSmeans(lmer_mode.mod)


