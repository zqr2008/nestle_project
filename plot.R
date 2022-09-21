library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(wesanderson)
library(ggsci)
library(extrafont)
library(pdftools)
library(ggthemes)
library(showtext)

loadfonts(device="win")
sysfonts::font_add_google("Roboto Condensed")
filter <- dplyr::filter

trajectory1 <- summary2  %>%
  filter(is.na(feeding_type_2)=="FALSE" & 
           is.na(tibia_sos) == "FALSE" &
           is.na(time) == "FALSE") 

trajectory2 <- summary2  %>%
  filter(is.na(tibia_sos) == "FALSE")

trajectory3 <- summary2  %>%
  filter(is.na(feeding_type_2)=="FALSE" & 
    is.na(tibia_length) == "FALSE")

trajectory4 <- summary2  %>%
  filter(is.na(tibia_length) == "FALSE")

pdf("mysave.pdf")
showtext_auto()

ggplot(trajectory2, aes(x = time, y = tibia_sos)) + 
  geom_point(size = 2 ,color ="#CB2314") + 
  geom_line(aes(group = PatientId),linetype=2, lty = 1, colour = "grey")+
  geom_smooth(colour = "#354823",alpha = 0.3) +
  theme_ipsum_rc() +
  xlab("time (days)") +
  ylab("tibia sos") +
  ggtitle("tibia sos trajectory for whole")+
  labs(fill = "gender")+
  scale_color_manual(values = wes_palette("BottleRocket2"))+
  theme(axis.title.x =element_text(size=16), axis.title.y=element_text(size=16))

ggplot(trajectory4, aes(x = time, y = tibia_length)) + 
  geom_point(size = 2,color ="#CB2314") + 
  geom_line(aes(group = PatientId), lty = 1, colour = "grey")+
  geom_smooth(colour = "#354823") +
  theme_ipsum_rc() +
  xlab("time (days)") +
  ylab("tibia sos") +
  ggtitle("tibia length trajectory for whole")+
  labs(fill = "gender")+
  scale_color_manual(values = wes_palette("BottleRocket2"))



ggplot(trajectory1, aes(x = time, y = tibia_sos ,
                               color = feeding_type_2)) + 
  geom_point(size = 2, position="dodge" ) + 
  geom_line(aes(group = PatientId), lty = 1, colour = "grey") +
  hrbrthemes::theme_ipsum_rc() +
  xlab("time (days)") +
  ylab("tibia sos") +
  ggtitle("tibia sos trajectory by feeding type")+
  labs(fill = "feeding type")+
  scale_color_manual(values = wes_palette("BottleRocket2"))



ggplot(trajectory2, aes(x = time, y = tibia_sos ,
                                color = SEX)) + 
  geom_point(size = 2) + 
  geom_line(aes(group = PatientId), lty = 1, colour = "grey")+
  theme_ipsum_rc() +
  xlab("time (days)") +
  ylab("tibia sos") +
  ggtitle("tibia sos trajectory by gender")+
  labs(fill = "gender")+
  scale_color_manual(values = wes_palette("BottleRocket2"))


 
ggplot(trajectory3, aes(x = time, y = tibia_length,
                                color = feeding_type_2)) + 
  geom_point(size = 2, position="dodge" ) + 
  geom_line(aes(group = PatientId), lty = 1, colour = "grey") +
  theme_ipsum_rc() +
  xlab("time (days)") +
  ylab("tibia length") +
  ggtitle("tibia length trajectory by feeding type")+
  labs(fill = "feeding type")+
  scale_color_manual(values = wes_palette("BottleRocket2"))

ggplot(trajectory4, aes(x = time, y = tibia_length,
                                color = SEX)) + 
  geom_point(size = 2, position="dodge" ) + 
  geom_line(aes(group = PatientId), lty = 1, colour = "grey") +
  theme_ipsum_rc() +
  xlab("time (days)") +
  ylab("tibia length") +
  ggtitle("tibia length trajectory by gender")+
  labs(fill = "gender")+
  scale_color_manual(values = wes_palette("BottleRocket2"))


z_para <- summary2 %>% 
  filter(health == "healthy") %>%
  group_by(time) %>%
  dplyr:: summarise(mean_length = mean(tibia_length, na.rm = T),
            mean_sos = mean(tibia_sos, na.rm = T),
         sd_length = sd(tibia_length, na.rm = T),
         sd_sos = sd(tibia_sos, na.rm =T ))  %>%
  dplyr:: right_join(summary2,by="time") %>%
  dplyr:: mutate(z_length = (tibia_length - mean_length)/sd_length,
                 z_sos = (tibia_sos - mean_sos)/sd_sos ) %>%
  filter(time <= 180)


ggplot(z_para, aes(x = time, y = z_length)) + 
  geom_point(size = 2, position="dodge",color = "red") + 
  geom_line(aes(group = PatientId), lty = 1, colour = "grey") +
  theme_ipsum_rc() +
  xlab("time (days)") +
  ylab("tibia length-for-age z-score") +
  ggtitle("tibia length-for-age z-score trajectory ")

 ggplot(z_para, aes(x = time, y = z_sos)) + 
  geom_point(size = 2, position="dodge",color = "orange") + 
  geom_line(aes(group = PatientId), lty = 1, colour = "grey") +
  theme_ipsum_rc() +
  xlab("time (days)") +
  ylab("tibia SoS-for-age z-score") +
  ggtitle("tibia SoS-for-age z-score trajectory ")


z_para_weight <- z_para %>%
  filter(is.na(feeding_type_2)=="FALSE")


ggplot(z_para_weight, aes(x = weight, y = tibia_sos,
                           color = feeding_type_2)) + 
  geom_point(size = 2, position="dodge") +
  geom_smooth(inherit.aes = FALSE,
              aes(x = weight, y = tibia_sos),
              colour = "#354823") +
  theme_ipsum_rc()+
  xlab("weight(kg)") +
  ylab("tibia SoS") +
  ggtitle("tibia SoS trajectory for weight")+
  scale_color_manual(values = wes_palette("BottleRocket2"))

dev.off()
save(summary2, file = "summary2.rda")
save(z_para, file = "z_para.rda")
