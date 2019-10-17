#3. #Tukey HSD + Fisher LSD
#Ruka
aov_ruka =  aov(Pocet ~ Ruka, data = dat)
summary(aov_ruka)
LSD1 <- LSD.test(aov_ruka, "Ruka");
LSD1
TukeyHSD(aov_ruka, ordered = FALSE, conf.level = alpha)
par(mfrow = c(1,2))
plot(LSD1)
plot(TukeyHSD(aov_ruka, ordered = FALSE,las=1))

#Kruh ##nemeli bychom ani testovat, protoze jsou ruzne variance ve skupinach 1cm, 3cm, 5cm
aov_kruh =  aov(Pocet ~ Kruh, data = dat)
summary(aov_kruh)
LSD1 <- LSD.test(aov_kruh, "Kruh");
LSD1
TukeyHSD(aov_kruh, ordered = FALSE, conf.level = alpha)
par(mfrow = c(1,2))
plot(LSD1)
plot(TukeyHSD(aov_kruh, ordered = FALSE,las=1))