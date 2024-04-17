
library(tidyverse)
library(data.table)
library(stringr)
library(lubridate)
theme_set(theme_light())

filenames = list.files("./processedData/travisData/allDat/", full.names = TRUE)
filenamesShort = list.files("./processedData/travisData/allDat/", full.names = FALSE)

for(i in 10:length(filenames)){
  batname = str_extract(filenamesShort[i], "[^.]+")
  tmp <- fread(filenames[i])
  tmpFilt <- tmp %>% filter(!is.na(VeDBA1min))
  write.csv(tmpFilt, file = paste0("./processedData/travisData/allDat/filter1min/", batname, ".csv"), row.names = FALSE)
  rm(tmp, tmpFilt)
}

filter1minFiles <- list.files("./processedData/travisData/allDat/filter1min/", full.names = TRUE)


allBats <- fread(filter1minFiles[1])

for(i in 2:length(filter1minFiles)){
  tmp <- fread(filter1minFiles[i])
  allBats <- bind_rows(allBats, tmp)
  rm(tmp)
}
write.csv(allBats, file = "AllBatsCombined1Min.csv", row.names = FALSE)


allBats <- fread("AllBatsCombined1Min.csv")
allBats$date <- date(allBats$Timestamp)
allBats$TagID <- as.factor(allBats$TagID)
allBats$behave <- as.factor(allBats$behave)

allBats %>% 
  filter(QI %in% (c(0,1))) %>% 
  ggplot()+
  geom_point(aes(y = HR_bpm, x = VeDBA1min, color = TagID), alpha = 0.6)+
  facet_wrap(~behave)+
  theme_bw()+
  scale_fill_viridis_d()


library(lme4)
library(car)
library(mgcv)

allBatsHQ <- allBats %>% filter(QI %in% (c(0,1)))

m1 <- lmer(HR_bpm~VeDBA1min + (1|TagID)+ (1|date), data = allBatsHQ)
m2 <- lmer(HR_bpm~VeDBA1min + (1|TagID), data = allBatsHQ)
Anova(m1)
summary(m1)
Anova(m2)
summary(m2)

library(mcgv)
library(tidymv)
g1 <- gamm(HR_bpm~s(VeDBA1min),
           random = list(TagID=~1), 
           data = allBatsHQ)
summary(g1$lme)
summary(g1$gam)
par(mfrow=c(2,2))
gam.check(g1$gam)
g1_pred <- predict.gam(g1$gam)

g1_pred %>% plot(series = "VeDBA1min")




#This doesn't do anything.
b1 <- bam(HR_bpm~s(VeDBA1min) + s(TagID, bs = "re"), 
           data = allBatsHQ)
b2 <- bam(HR_bpm~s(VeDBA1min, by = behave, k=6) + s(TagID, bs = "re"), 
          data = allBatsHQ)

b3 <- bam(HR_bpm~s(VeDBAmean) + s(TagID, bs = "re"), 
          data = allBatsHQ)
b3_pred <- predict_gam(b3)
b3_pred %>% plot(series = "VeDBAmean")
b3_pred %>% plot("VeDBAmean", "TagID")

b1_pred <- predict_gam(b1)
b1_pred %>% plot(series = "VeDBA1min")
b1_pred %>% plot("VeDBA1min", "TagID")
b1_pred %>% plot(select = 3)

ggplot(allBatsHQ)+
  geom_point(aes(x = VeDBA1min, y = HR_bpm, color = TagID), alpha = 0.5)+
  stat_smooth(aes(x = VeDBA1min, y = HR_bpm), data = b1_pred)+
  theme(legend.position = "none")
  


b2_pred <- predict_gam(b2)
b2_pred %>% plot(series = "VeDBA1min")


plot_smooth(b1_pred, view="VeDBA1min", cond=list(Group="TagID"), 
            rm.ranef=TRUE, main="intercept+s(VeDBA1min)", rug=FALSE)



ares <- fread("processedData/travisData/allDat/AresTotal29.csv")
hist(ares$flight_duration)
           