library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(wesanderson)
library(ggsci)

filter <- dplyr::filter

summary2$SEX<-factor(summary2$SEX,levels = c("1","2"),
                     labels = c("male","female"))

trajectory1 <- summary2  %>%
  mutate_at(.vars = vars(3:6),.funs = as.numeric) %>%
  filter(is.na(feeding_type_2)=="FALSE" & 
           is.na(tibia_sos) == "FALSE")

trajectory2 <- summary2  %>%
  mutate_at(.vars = vars(3:6),.funs = as.numeric) %>%
  filter(is.na(tibia_sos) == "FALSE")

trajectory3 <- summary2  %>%
  mutate_at(.vars = vars(3:6),.funs = as.numeric) %>%
  filter(is.na(feeding_type_2)=="FALSE" & 
    is.na(tibia_length) == "FALSE")

trajectory4 <- summary2  %>%
  mutate_at(.vars = vars(3:6),.funs = as.numeric) %>%
  filter(is.na(tibia_length) == "FALSE")


pic1 <- ggplot(trajectory1, aes(x = time, y = tibia_sos ,
                               color = feeding_type_2)) + 
  geom_point(size = 2, position="dodge" ) + 
  geom_line(aes(group = PatientId), lty = 1, colour = "grey") +
  theme_ipsum() +
  xlab("time (days)") +
  ylab("tibia sos") +
  ggtitle("tibia sos trajectory by feeding type")+
  labs(fill = "feeding type")+
  scale_color_manual(values = wes_palette("BottleRocket2"))
  

pic2 <-  ggplot(trajectory2, aes(x = time, y = tibia_sos ,
                                color = SEX)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = PatientId), lty = 1, colour = "grey")+
  theme_ipsum() +
  xlab("time (days)") +
  ylab("tibia sos") +
  ggtitle("tibia sos trajectory by gender")+
  labs(fill = "feeding type")+
  scale_color_manual(values = wes_palette("BottleRocket2"))


pic3 <- ggplot(trajectory3, aes(x = time, y = tibia_length,
                                color = feeding_type_2)) + 
  geom_point(size = 2, position="dodge" ) + 
  geom_line(aes(group = PatientId), lty = 1, colour = "grey") +
  theme_ipsum() +
  xlab("time (days)") +
  ylab("tibia length") +
  ggtitle("tibia length trajectory by feeding type")+
  labs(fill = "feeding type")+
  scale_color_manual(values = wes_palette("BottleRocket2"))

pic4 <- ggplot(trajectory4, aes(x = time, y = tibia_length,
                                color = SEX)) + 
  geom_point(size = 2, position="dodge" ) + 
  geom_line(aes(group = PatientId), lty = 1, colour = "grey") +
  theme_ipsum() +
  xlab("time (days)") +
  ylab("tibia length") +
  ggtitle("tibia length trajectory by gender")+
  labs(fill = "gender")+
  scale_color_manual(values = wes_palette("BottleRocket2"))

