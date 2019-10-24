# NEX - Homework 01
# Written by J. Franc - jiri.franc@fjfi.cvut.cz


# Required libraries (Maybe baby)
library(car)          # provides a set of useful functions for ANOVA designs and Regression Models;
library(lattice)      # provides some graphical enhancements compared to traditional R graphics, as well as multivariate displays capabilities;
library(lme4)         # the newer and enhanced version of the nlme package, for which additional data structure are available (nested or hierarchical model,. . . );
library(nlme)         # for handling mixed-effects models;
library(pwr)          # power analysis
library(agricolae)    # for Fisher LSD method
library(scatterplot3d)# for 3d scatter plot
library(nortest)      # for lillie.test
library(lmtest)       # for bptest
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)

getwd()  # find where are you at the moment 


# HOMEWORK
#
# Dot in circle problem
#
#
# UKOL: Zjistete jaky má vliv velikost kruhu a použití ruky kterou experiment provadime
#          (leva, prava, obe) na počet teček uvnitr kruhu za 10s (5s)
# Provedení: Využijte šablonu a po dobu 10 sekund (stopuje vam kolega) 
#              se snazte udelat na stridacku do obou kruhu co nejvice tecek.
#            (snazte se trefovat a delat tecky, ne cary pres pulku kruhu. 
#              muzete se treba penalizovat za tecku mimo apod.)
#
# 1) namerte data: experiment nastavte tak, aby byl znahodneny !!!!!
#                  poradi mereni si poznamenejte pro dalsi kontrolu pripadne zavislosti na poradi mereni
#                  Jednotlive lidi ve skupine berte jako ruzne operatory experimentu (blokujte)
#                  Pocet replikaci u jednoho cloveka a jednoho casu vemte 1
#                  V protokolu krom popisu experimentu diskutujte i jen premenne ktere mohli mit na mereni vliv
#                  
#
#  2)  Spoctete zakladni statistiky (mean, median a sd pro jednotlive faktory - velikost, ruka, operator)
#      Zobrazte namerena data (box plot, interaction plot, effects plot, ...) a okomentujte je
#      co z danych obrazku muzeme pred samotnou analyzou rici o vysledku?
#  
#   3) Zamerte se zvlaste na faktory ruka a velikost kola. Otestujte hypotezu o schodnosti rozptylu pro jednotlive urovne
#      a vhodnym testem overte stejnost strednich hodnot. 
#      Provedte Tukey HSD a Fisher LSD test pro parove porovnani stednich hodnot jednotlivych skupin s vybranou korekci phodnoty.
#
#   4) Analyzute data pomoci ANOVA (vsechny promenne berte jako faktor)
#          - s predpokladem, ze kazdy z ucastniku experimentu tvori jeden blok.
#          - diskutujte vysledky, overte predpoklady, vykreslete QQ-plot, residua x fitted values, resida x cas, ...
#          - diskutujte vliv znahodneni experimentu a vyvoj rezidui v case (cislo mereni)
#
#   5) Porovnejte a diskutujte vysledky z bodu 3 a 4.
#
#   6) Pokud data nesplnuji predpoklady pro pouziti ANOVA, diskutujte mozne transformace (logaritmicka, Box-Cox, ...)
#      a duvod proc data predpoklady nesplnuji? Vyskyt outlieru, zpusob mereni, divny operator, ...
#
#   7) Vyberte nejvhodnejsi model a i kdyby nesplnoval predpoklady pro pouziti ANOVA reste nasledujici: 
#   - Spoctete silu testu v ANOVA (pro max. dvoufaktorovou analyzu
#                                  - jeden z faktoru zanedbejte, nebo vezmete mereni pro jednu jeho konkretni uroven)
#   - Predpokladejme, ze standartni odchylka disturbanci bude pro provadeny experiment 4
#     a maximalni (pro nas signifikantni) rozdil, ktery chceme detekovat je 5 bodu v kruhu.
#     Spoctete pocet potrebnych replikaci, aby sila vysledneho testu byla vetsi nez 0.9
# 
#   8) Vytvorte regresni model, kde nebudete uvazovat bloky a velikost kruhu bude kvantitativni promenna.
#      - zkuste pridat do modelu i druhou mocninu a porovnejte dva regresni modely mezi s sebou a vyberte vhodnejsi
#      - overte predpoklady pro pouziti vybraneho modelu a vykreslete QQ-plot, residua x fitted values, resida x prumer , ...
#

#   Pozn: V pripade, ze bloky jsou velmi dominantni a zadna z dalsich promennych se nejevy vyznamna,
#      pouzijte pro vyslednou analyzu jen "operatora s daty nejlepsimi pro akademicke ucely".
#
#
#    Pozn: rozsireni ulohy, zmerte jeste zavislost na case. (5s, 10s, 15s)


# Define directory

fileName <- 'path.txt'
path = readChar(fileName, file.info(fileName)$size)
setwd(path)

# Read data
dat <- read.table("Data/ukol_adamkajetan.csv",header=TRUE,sep=",")

colnames(dat)[1] <- "Poradi"
dat$Kruh = factor(dat$Kruh)

#### 2) ######
#zakladni stat.
summary(dat)

adam <- dat[which(dat$Jmeno =="adam"),]
kajetan <- dat[which(dat$Jmeno =="kajetan"),]
dominant <- dat[which(dat$Ruka =="dominant"),]
nondominant <- dat[which(dat$Ruka =="nondominant"),]
joint <- dat[which(dat$Ruka =="joint"),]
one_cm <- dat[which(dat$Kruh == 1),]
three_cm <- dat[which(dat$Kruh == 3),]
five_cm <- dat[which(dat$Kruh == 5),] 

#Adam
characteristics <- {} #initialization
statistic <- summary(adam$Pocet)
statistic[7] <-sd(adam$Pocet)
characteristics <- cbind(adam = statistic)

#Kajetan
statistic <- summary(kajetan$Pocet)
statistic[7] <-sd(kajetan$Pocet)
characteristics <- cbind(characteristics, kajetan = statistic)

#Dominant
statistic <- summary(dominant$Pocet)
statistic[7] <-sd(dominant$Pocet)
characteristics <- cbind(characteristics, dominant = statistic)

#Nondominant
statistic <- summary(nondominant$Pocet)
statistic[7] <-sd(nondominant$Pocet)
characteristics <- cbind(characteristics, nondominant = statistic)

#Joint
statistic <- summary(joint$Pocet)
statistic[7] <-sd(joint$Pocet)
characteristics <- cbind(characteristics, joint = statistic)

#1 cm
statistic <- summary(one_cm$Pocet)
statistic[7] <-sd(one_cm$Pocet)
characteristics <- cbind(characteristics, "1 cm" = statistic)

#3 cm
statistic <- summary(three_cm$Pocet)
statistic[7] <-sd(three_cm$Pocet)
characteristics <- cbind(characteristics, "3 cm" = statistic)

#5 cm
statistic <- summary(five_cm$Pocet)
statistic[7] <-sd(five_cm$Pocet)
characteristics <- cbind(characteristics, "5 cm" = statistic)

attributes(characteristics)$dimnames[[1]][7] <- "Sd." #name of last row
characteristics

#BOXPLOTS

#boxplot(Pocet~Kruh, dat)
#boxplot(Pocet~Poradi, dat)
#boxplot(Pocet~Jmeno, dat)
#boxplot(Pocet~Ruka, dat)
#boxplot(Pocet~Kruh+Ruka, dat)
#boxplot(Pocet~Jmeno+Kruh, dat)
#boxplot(Pocet~Ruka+Jmeno, dat)

require(ggplot2)
require(gridExtra)


p1 <- ggplot(dat, aes(x=Ruka, y=Pocet, color = Jmeno)) + 
  geom_boxplot()

p2 <- ggplot(dat, aes(x=Kruh, y=Pocet, color = Jmeno)) + 
  geom_boxplot()

p3 <- ggplot(dat, aes(x=Ruka, y=Pocet)) + 
  geom_boxplot()

p4 <- ggplot(dat, aes(x=Jmeno, y=Pocet)) + 
  geom_boxplot()

p5 <- ggplot(dat, aes(x=Kruh, y=Pocet)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=3,
               outlier.size=4)

#tyto dva bych dal do .Rmd
grid.arrange(p1,p2,ncol=2) 
grid.arrange(p4,p3,p5,ncol=3)
#p5


ggplot(dat) +
  aes(x = Jmeno, y = Pocet) +
  geom_boxplot() +
  facet_wrap(~Ruka)

ggplot(dat) +
  aes(x = Ruka, y = Pocet) +
  geom_boxplot() +
  facet_wrap(~Kruh)

ggplot(dat) +
  aes(x = Jmeno, y = Pocet) +
  geom_boxplot() +
  facet_wrap(~Kruh)

#INFLUENCE PLOTS
require(dplyr)

dat %>%
  group_by(Jmeno, Ruka) %>% 
  summarise(grPocet = mean(Pocet)) -> dat2


dat2 %>% 
  ggplot() +
  aes(x = Jmeno, y = grPocet, color = Ruka) +
  geom_line(aes(group = Ruka)) +
  geom_point() + labs(x = "Jméno", y = "Počet") + ggtitle("Influence plot - Počet x Jméno")

dat %>%
  group_by(Jmeno, Kruh) %>% 
  summarise(grPocet = mean(Pocet)) -> dat3


dat3 %>% 
  ggplot() +
  aes(x = Jmeno, y = grPocet, color = Kruh) +
  geom_line(aes(group = Kruh)) +
  geom_point() + labs(x = "Kruh", y = "Počet") + ggtitle("Influence plot - Počet x Kruh")

dat %>% 
  group_by(Kruh, Ruka) %>%
  summarise(grPocet = mean(Pocet)) -> dat4

dat4 %>%
  ggplot() + aes(x = Ruka, y = grPocet, color = Kruh) + 
  geom_line(aes(group = Kruh)) +
  geom_point() + labs(x = "Ruka", y = "Počet") + ggtitle("Influence plot - Ruka x Kruh")


#EFFECT PLOTS
plot.design(Pocet~Ruka+Kruh+Jmeno, data = dat, main = "Effect plot")



#### 3) ######
#hypoteza shodnosti rozptylu pro jednotlive urovne
alpha = 0.95

##jmeno: asi nebudeme potrebovat, mame se zamerit na ruka a velikost kola

# podminky f-testu: normalita, tj. histogram a shapiro-wilk test
par(mfrow = c(1,2))
hist(adam$Pocet, breaks=6)
hist(kajetan$Pocet, breaks=6)
shapiro.test(adam$Pocet)
shapiro.test(kajetan$Pocet) # p-hodnoty > 0.05, tj. nezamitame normalitu
var.test(adam$Pocet, kajetan$Pocet, ratio = 1, alternative = "two.sided", conf.level = alpha) # p-hodnota > 0.05, tj. nezamitame rovnost rozptylu

##ruka
par(mfrow = c(1,3))
hist(dominant$Pocet)
hist(nondominant$Pocet)
hist(joint$Pocet)
shapiro.test(dominant$Pocet)
shapiro.test(nondominant$Pocet)
shapiro.test(joint$Pocet) # p-hodnoty > 0.05, tj. nezamitame normalitu

var.test(dominant$Pocet, nondominant$Pocet, ratio = 1, alternative = "two.sided", conf.level = alpha) # p-hodnota > 0.05, tj. nezamitame rovnost rozptylu
var.test(dominant$Pocet, joint$Pocet, ratio = 1, alternative = "two.sided", conf.level = alpha) # p-hodnota > 0.05, tj. nezamitame rovnost rozptylu
var.test(nondominant$Pocet, joint$Pocet, ratio = 1, alternative = "two.sided", conf.level = alpha) # p-hodnota > 0.05, tj. nezamitame rovnost rozptylu

##kruh
par(mfrow = c(1,3))
hist(one_cm$Pocet)
hist(three_cm$Pocet)
hist(five_cm$Pocet)
shapiro.test(one_cm$Pocet)
shapiro.test(three_cm$Pocet)
shapiro.test(five_cm$Pocet) # p-hodnoty > 0.05, tj. nezamitame normalitu

var.test(one_cm$Pocet, three_cm$Pocet, ratio = 1, alternative = "two.sided", conf.level = alpha) # ZAMITAME!!!
var.test(one_cm$Pocet, five_cm$Pocet, ratio = 1, alternative = "two.sided", conf.level = alpha) # ZAMITAME!!!
var.test(three_cm$Pocet, five_cm$Pocet, ratio = 1, alternative = "two.sided", conf.level = alpha)

# hypoteza shodnosti strednich hodnot
# podminky t-testu: normalita, tj. histogram a shapiro-wilk test; uz jsme provadeli
t.test(adam$Pocet, kajetan$Pocet, conf.level = 0.95, alternative = "two.sided", var.equal = TRUE) #nezamitame

### toto bude lepsi nahradit pairwise testem
t.test(dominant$Pocet, nondominant$Pocet, conf.level = 0.95, alternative = "two.sided", var.equal = TRUE) #nezamitame
t.test(dominant$Pocet, joint$Pocet, conf.level = 0.95, alternative = "two.sided", var.equal = TRUE) #nezamitame
t.test(nondominant$Pocet, joint$Pocet, conf.level = 0.95, alternative = "two.sided", var.equal = TRUE) #nezamitame

t.test(one_cm$Pocet, three_cm$Pocet, conf.level = 0.95, alternative = "two.sided", var.equal = FALSE) #var.equal = FALSE pouziva Welchovu aproximaci
t.test(one_cm$Pocet, five_cm$Pocet, conf.level = 0.95, alternative = "two.sided", var.equal = FALSE) 
t.test(three_cm$Pocet, five_cm$Pocet, conf.level = 0.95, alternative = "two.sided", var.equal = TRUE) #vsechny jasne zamitame
### toto bude lepsi nahradit pairwise testem

pairwise.t.test(dat$Pocet,dat$Ruka,p.adjust.method="bonferroni")
pairwise.t.test(dat$Pocet,dat$Ruka,p.adjust.method="hochberg")

pairwise.t.test(dat$Pocet,dat$Kruh,p.adjust.method="bonferroni")
pairwise.t.test(dat$Pocet,dat$Kruh,p.adjust.method="hochberg")

#Tukey HSD + Fisher LSD
#Celkovy aov
#Kruh bychom ani testovat, protoze jsou ruzne variance ve skupinach 1cm, 3cm, 5cm
aov_celk =  aov(Pocet ~ Jmeno + Ruka + Kruh, data = dat) #ruka neni stat. vyznamna
LSD1 <- LSD.test(aov_celk, "Jmeno"); LSD1
LSD2 <- LSD.test(aov_celk, "Ruka"); LSD2
LSD3 <- LSD.test(aov_celk, "Kruh"); LSD3
TukeyHSD(aov_celk, "Jmeno", ordered = FALSE, conf.level = alpha)
TukeyHSD(aov_celk, "Ruka", ordered = FALSE, conf.level = alpha)
TukeyHSD(aov_celk, "Kruh", ordered = FALSE, conf.level = alpha)

par(mfrow = c(2,3))
plot(LSD1)
plot(LSD2)
plot(LSD3)
plot(TukeyHSD(aov_celk, "Jmeno", ordered = FALSE,las=1))
plot(TukeyHSD(aov_celk, "Ruka", ordered = FALSE,las=1))
plot(TukeyHSD(aov_celk, "Kruh", ordered = FALSE,las=1))


#### 4) ###### neco z toho uz je ve 3), Jirka to zadava napreskacku
summary(aov_celk)
par(mfrow = c(2,2))
plot(aov_celk)

leveneTest(aov_celk)

#bartlett.test(Pocet ~ Jmeno + Kruh, data = dat)
#bartlett.test(Pocet ~ Kruh, data = dat)
#bartlett.test(Pocet ~ Ruka, data = dat)
#leveneTest(Pocet ~ Jmeno, data = dat)
#leveneTest(Pocet ~ Kruh, data = dat)
#leveneTest(Pocet ~ Ruka, data = dat)

lillie.test(resid(aov_celk))
shapiro.test(resid(aov_celk))
bptest(aov_celk)

#Ruka je nevyznamna, stejne tak ukazuje LSD a HSD => odstranme ji
aov_celk2 =  aov(Pocet ~ Jmeno + Kruh, data = dat) #ruka neni stat. vyznamna
summary(aov_celk2)

#Zkusme i interakci mezi Jmeno a Kruh => ## FINALNI MODEL
aov_celk3 =  aov(Pocet ~ Jmeno * Kruh, data = dat) #ruka neni stat. vyznamna, jiz neuvazujeme
summary(aov_celk3)
plot(aov_celk3)
lillie.test(residuals(aov_celk3))
shapiro.test(resid(aov_celk3))
bptest(aov_celk3)

#Zkusme, zdali nebude mit Poradi vliv
aov_celk4 =  aov(Pocet ~ Jmeno * Kruh + Poradi, data = dat) #ruka neni stat. vyznamna
summary(aov_celk4) #Poradi ma p-hodnotu 0.084
plot(aov_celk4)
lillie.test(residuals(aov_celk4))
shapiro.test(resid(aov_celk4))
bptest(aov_celk4)
leveneTest(aov_celk4)

#Porovnat dva nested modely; jeden obsahuje Poradi, druhy ne
anova(aov_celk3, aov_celk4) # > 0.05, tj. nezamitame a lepsi je model aov_celk3: Pocet ~ Jmeno * Kruh


#### 5) ###### 

#### 6) ###### 
#zde bude problem ta variance u kruhu, 
#NE! Neni problem; v plotech je to OK a bptest() taky vychazi

#### 7) ######

# vybirame asi Pocet ~  Jmeno*Kruh
anova_fin <- aov(Pocet ~ Jmeno*Kruh, data = dat)
summary(anova_fin)
shapiro.test(anova_fin$residuals) # OK...zamitame
bptest(anova_fin)
#leveneTest(anova_fin) #OK....zamitame

mean1 <- mean(dat$Pocet[which(dat$Jmeno=="kajetan")])
mean2 <- mean(dat$Pocet[which(dat$Jmeno=="adam")])
means <- c(mean1, mean2) 

power.anova.test(groups = 2, between.var = var(means), n = length(dat$Pocet[which(dat$Jmeno=="kajetan")]) , sig.level = 0.05, within.var = 16)

# kolik potrebnych replikaci pro power > 0,9
power.anova.test(groups = 2, between.var = var(means), n = NULL , sig.level = 0.05, within.var = 16, power = 0.9) #alespon 11 replikaci

n <- c(seq(2,10,by=1),seq(12,20,by=2),seq(25,50,by=5))
pow <- power.anova.test(groups = 2, between.var = var(means), n = n , sig.level = 0.05, within.var = 16)
plot(n, pow$power, xlab = "Počet replikací", ylab = "Síla testu", main = "Síla testu v závislosti na počtu replikací", col= "red", type = "o")

# Za finální ANOVA model jsme na základě signifikance proměnných a hodnotě R^2 vybrali model Pocet ~ Jmeno*Kruh. Tento model splňuje 
# podmínky ANOVA. Konkrétně, na základě p-hodnoty Shapirova-Wilkova testu nezamítáme normalitu reziduí na 5% hladině významnosti a 
# stejně tak na základě Breuschova-Paganova testu nezamítáme homoskedasticitu.
# Pro výpočet síly testu bereme 5% hladinu významnosti. Předpokládáme, že statistická odchylka disturbancí pro experiment je 4 (rozptyl = 16).
# Síla testu pro dvouúrovňový faktor Jméno, kde máme 9 pozorování, je 0.834. Pro sílu testu větší než 0.9 bychom potřebovali alespoň 
# 11 replikací. Tato skute4nost je viditelné i z grafu závisloti síly testu na počtu replikací.
#
#

 


#### 8) #### 
dat$Kruh <- as.numeric(dat$Kruh)

linmod <- lm(Pocet ~ Kruh, data = dat)
summary(linmod)

linmod2 <- lm(Pocet ~ Kruh + I(Kruh^2), data = dat)
summary(linmod2)#druha mocnina ani neni vyznamna, takze nema smysl dal tento model uvazovat
par(mfrow = c(2,2))
plot(linmod)
lillie.test(residuals(linmod))
shapiro.test(resid(linmod))
bptest(linmod) #bp zamita, mame heteroskedasticitu

#Final model => linmod_log
linmod_log <- lm(log(Pocet) ~ Kruh, data = dat)
summary(linmod_log)
plot(linmod_log)
lillie.test(residuals(linmod_log))
shapiro.test(resid(linmod_log))
bptest(linmod_log) 
#jiz je vse v poradku; testy vychazeji i graficky by se to dalo ukecat
#navic i intercept je vyznamny, takze bonus navic; R^2_adj 0.85, to vypada fajn
