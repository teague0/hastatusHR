library(data.table)
library(tidyverse)
library(lubridate)
bats <- fread("./processedData/bats.csv")

bats$date <- date(bats$Timestamp)
bats$TagID <- as.factor(bats$TagID)
bats$behave <- as.factor(bats$behave)


library(lme4)
library(car)
library(mgcv)
library(tidygam)



allBatsHQ <- bats %>% filter(QI %in% (c(0,1)))

noQuickRest <- allBatsHQ %>% filter(rest_duration > 59 | flight_duration > 30)

nightOnly <- noQuickRest %>% filter(hour(Timestamp) > 17 | hour(Timestamp) < 7)


ggplot(noQuickRest)+
  geom_point(aes(y = HR_bpm, x = VeDBA1min, color = TagID), alpha = 0.6)


m1 <- lmer(HR_bpm~VeDBA1min + behave+duration + (1|TagID), data = noQuickRest)

m2 <- lmer(HR_bpm~VeDBA1min + (1|TagID), data = allBatsHQ)
Anova(m1)
summary(m1)
Anova(m2)
summary(m2)


#This doesn't do anything.
b1 <- bam(HR_bpm~s(VeDBA1min) + s(TagID, bs = "re"), 
           data = noQuickRest)
b2 <- bam(HR_bpm~s(VeDBA1min, by = behave, k=6) + s(TagID, bs = "re"), 
          data = noQuickRest)

b3 <- bam(HR_bpm~s(VeDBAmean, by = behave, k=10) + s(TagID, bs = "re"), 
          data = allBatsHQ)
b3_pred <- predict_gam(b3)
b3_pred %>% plot(series = "VeDBAmean")
b3_pred %>% plot("VeDBAmean", "TagID")

b1_pred <- predict_gam(b1)
b1_pred %>% plot(series = "VeDBA1min")
b1_pred %>% plot("VeDBA1min", "TagID")
b1_pred %>% plot(select = 3)

ggplot(noQuickRest)+
  geom_point(aes(x = VeDBA1min, y = HR_bpm, color = TagID), alpha = 0.5)+
  stat_smooth(aes(x = VeDBA1min, y = HR_bpm), data = b1_pred)+
  theme(legend.position = "none")
  


b2_pred <- predict_gam(b2)
b2_pred %>% plot(series = "VeDBA1min")


plot_smooth(b1_pred, view="VeDBA1min", cond=list(Group="TagID"), 
            rm.ranef=TRUE, main="intercept+s(VeDBA1min)", rug=FALSE)

#Travis sent 4/24

library(tidygam)
allBatsHQ <- bats %>% filter(QI %in% (c(0,1)))
nightOnly <- allBatsHQ %>% filter(hour(Timestamp) > 17 | hour(Timestamp) < 7)
noQuickRest2 <- nightOnly %>% filter(rest_duration > 59 | flight_duration > 30)
gam_model6 <- gamm(HR_bpm ~ s(VeDBA1min, by = behave, k=6) + 
                     s(airspeed) +
                     s(TagID, bs = "re"),
                   data = noQuickRest2)
gam_model6 <- bam(HR_bpm ~ s(VeDBA1min, by = behave, k=6) + 
                    s(TagID, bs = "re"),
                  data = noQuickRest2)
summary(gam_model6)
gam_model6_pred <- predict_gam(gam_model6)
gam_model6_pred %>% plot(series = "VeDBA1min")
gam_model6_pred %>% plot("VeDBA1min", "behave")


gam_model7 <- bam(HR_bpm ~ s(VeDBA1min, by = behave, k=4) + 
                     s(airspeed) +
                     s(TagID, bs = "re"),
                   data = noQuickRest2)
summary(gam_model7)

gam_model6 <- bam(HR_bpm ~ s(VeDBA1min, by = behave, k=10) + 
                    s(TagID, bs = "re"),
                  data = noQuickRest2)
summary(gam_model6)




gam_model7.4 <- bam(HR_bpm ~ s(VeDBA1min, by = behave, k=4) + 
                    s(airspeed) +
                    s(TagID, bs = "re"),
                  data = noQuickRest2)
summary(gam_model7.4)

par(mar = c(5, 5, 2, 2))
plot(gam_model6$gam, select = 1)
summary(gam_model6$gam)


gam_model7 <- bam(HR_bpm ~ s(VeDBA1min, by = behave, k=6) + 
                   s(airspeed) +
                   s(TagID, bs = "re"),
                 data = noQuickRest2)
summary(gam_model7)
gam_m7_pred <- predict_gam(gam_model7)
gam_m7_pred %>% plot(series = "VeDBA1min", "TagID")
gam_m7_pred %>% plot("VeDBA1min", "behave")
gam_m7_pred %>% plot(select = 3)

gam_model8 <- bam(VO2 ~ s(VeDBA1min, by = behave, k=6) + 
                    s(airspeed) +
                    s(TagID, bs = "re"),
                  data = noQuickRest2)
summary(gam_model8)
gam_m8_pred <- predict_gam(gam_model8)
gam_m8_pred %>% plot(series = "VeDBA1min", "TagID")
gam_m8_pred %>% plot("VeDBA1min", "behave")
gam_m8_pred %>% plot(select = 3)




ares <- fread("processedData/travisData/allDat/AresTotal29.csv")
hist(ares$flight_duration)


tmp <- fread(filenames[5])
hist(tmp$rest_duration, breaks = "FD")

ggplot(noQuickRest)+
  geom_boxplot(aes(x = behave, y = VeDBA1min, group = behave))
           

ggplot(allBatsHQ)+
  geom_boxplot(aes(x = hour(Timestamp), y = HR_bpm, group = hour(Timestamp)))

ggplot(allBatsHQ)+
  geom_boxplot(aes(x = hour(Timestamp), y = temp, group = hour(Timestamp)))
         