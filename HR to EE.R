#This uses the Bishop & Spivey 2013 (https://doi.org/10.1016/j.jtbi.2013.01.002) relationship between body mass, heart mass, and heart rate to estimate instantaneous energy expenditure.
#VO2 (mL O2 min-1) = 0.042 * Mb(kg)0.328 * Mh(kg)0.913 * fh^2.065
#VO2 = oxygen consumptions (mL per min)
#Mb = body mass (kg)
#Mh = heart mass (kg). This is estimated from the literature or from known values
#fh = heart rate (bpm)

dat$VO2 <- 0.042 * dat$Mb^0.328 * dat$Mh^0.913 * dat$hr.est^2.065 
dat$joul <- dat$VO2 * 21.11 #convert VO2 to Joules per minute 1 mLO2 = 21.11 J
dat$KJPH <- (dat$joul*60)/1000 #convert joules (watts per second) to kilojoules per hour



#We can also estimate instantaneous metabolic power output from airspeed by first calculating mechanical power based on Pennycuick's model, then doing a couple of additional conversions to get metabolic power. This was used in O'Mara & Dechmann 2023 (https://doi.org/10.1016/j.anbehav.2023.08.001)

#Calculate metabolic power based on equation 1 from Ward et al 2001 (https://doi.org/10.1242/jeb.204.19.3311). 

#First, Get pennycuick power estimates. This will calculate a power estimate for every GPS fix where an airspeed has been calculated (ground speed + wind support)

source("./script/pennypower_function.R")
m <- dat$mass.deploy/1000 #mass in kg
wingspan <- dat$winglen.mm*2/1000 #length of both wings in m
altitude <- dat$height.above.msl #altitude
airspeed <- dat$airspeedRecalc #airspeed in m/s
pwr <- numeric(length = length(m)) #create an empty vector for power estimates
for(i in 1:length(m)){
  pwr[i] <- pennypower(m[i], wingspan[i], altitude[i], airspeed[i])
}
dat$pwrPenny <- pwr

Pmet <- numeric(length = length(m)) #create empty vector for the metabolic power

#Ep is the partial efficiency of the muscle. From Thomas 1975 (https://doi.org/10.1242/jeb.63.1.273) for P. hastatus this ranges between mean value of 0.13 - 0.34. 0.23 or 0.18 are the bird values (Ward 2001). 
Ep <- 0.2466667
Pmet <-  1.1*(((pwr)/Ep)+(23.8 / 360))
dat$Pmet <- Pmet

#Phyllostomus metabolic power curves -- Thomas 1975 between 6-9 m/s
# 0 degrees P (W/kg) = 311.98 - 63.08V + 4.54V^2
#metPwr <- (311.98 - 63.08*airspeed + 4.54*airspeed^2)*m

## Incorporate resting metabolic rate from McNab 1969 (https://doi.org/10.1016/0010-406X(69)91651-X) into the resting / non-movement data. 
#McNab 1969: 84.2 g; 1.19 ccO2/g/hr @ 34.7 degrees
# 1 mL O2 = 20 J
# 23.8 J/g/h - divide by 360 to get W = 0.006611111 W / g

#This is specific to O'Mara & Dechmann 2023, but tny time lag over 90 s (the sleep time for missed fixes -- GPS went to sleep for 300 s, then searched for 90 s) I'm going to assume to be roosting somewhere. 

sleepLag <- which(dat$tlag > 90)
dat$newState[sleepLag] <- "state1"
state1s <- which(dat$newState == "state1")
dat$PmetTotal <- dat$Pmet * dat$tlag #convert W (J/s) to just joules
dat$PmetTotal[state1s] <- (23.8 / 360) * dat$mass.deploy[state1s]* dat$tlag[state1s]

#save(dat, file="./data/11_HastatusSegStateBiodatPwr.Rdata")

#Just for fun -- exploration of Speakman & Thomas 2003 DEE estimates via body mass scaling.
#Speakman & Thomas 2003
#Rest: log(BMR ml O2 per hour) = 1.08985 + 0.744 * logMb (g)
#Flight: Pf (W) = -1.46 + 0.744 * Mb (g)

#Speakman 2005
#ln DEE (KJ per day) = 2.05 + 0.621 * ln (Mb(g))

logSpeak <- 2.05 + 0.621 * log(phbiodat$mass.deploy)
deeSpeak <- exp(logSpeak)

bat.mass$log.Speak.KJ= 2.05 + 0.621 * log(bat.mass$Mb*1000)
bat.mass$Speak.KJ=exp(bat.mass$log.Speak.KJ)
plot(Speak.KJ~log(Mb*1000), data=bat.mass)