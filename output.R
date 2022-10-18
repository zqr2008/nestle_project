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


