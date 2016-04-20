


setwd("D:/UnivAutBarcelona/SNA colombia/Migración Redes Colombia")

library(sp)
library(RColorBrewer)
library(ggplot2)
library(maptools)
library(scales)
library(rgeos)        
library(latticeExtra) 
library(classInt)
library(data.table)
library(dplyr)
library(mapdata)

muni <- read.csv('Municipalities.csv',header=T,sep=';',dec='.')
muni <- tbl_df(muni)

#test <- read.csv('IndicatorsTest.csv',header=T,sep=';',dec='.')
#test <- tbl_df(test)
#str(test)

cl5 <- tbl_df(read.csv('cluster5.csv',header=T,sep=';',dec='.'))
cl6 <- tbl_df(read.csv('cluster6.csv',header=T,sep=',',dec='.'))
cl7 <- tbl_df(read.csv('cluster7.csv',header=T,sep=';',dec='.'))
cl8 <- tbl_df(read.csv('cluster8.csv',header=T,sep=',',dec='.'))
cl9 <- tbl_df(read.csv('cluster9.csv',header=T,sep=',',dec='.'))
cl10 <- tbl_df(read.csv('cluster10.csv',header=T,sep=',',dec='.'))
cl11 <- tbl_df(read.csv('cluster11.csv',header=T,sep=',',dec='.'))
cl12 <- tbl_df(read.csv('cluster12.csv',header=T,sep=',',dec='.'))
cl13 <- tbl_df(read.csv('cluster13.csv',header=T,sep=',',dec='.'))
cl14 <- tbl_df(read.csv('cluster14.csv',header=T,sep=',',dec='.'))
cl15 <- tbl_df(read.csv('cluster15.csv',header=T,sep=',',dec='.'))
cl16 <- tbl_df(read.csv('cluster16.csv',header=T,sep=',',dec='.'))
cl17 <- tbl_df(read.csv('cluster17.csv',header=T,sep=',',dec='.'))
cl18 <- tbl_df(read.csv('cluster18.csv',header=T,sep=',',dec='.'))
cl19 <- tbl_df(read.csv('cluster19.csv',header=T,sep=',',dec='.'))
cl20 <- tbl_df(read.csv('cluster20.csv',header=T,sep=',',dec='.'))


muni.new <- inner_join(muni, cl5, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl6, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl7, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl8, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl9, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl10, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl11, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl12, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl13, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl14, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl15, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl16, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl17, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl18, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl19, by = c("Municipios" = "Id"))
muni.new <- inner_join(muni.new, cl20, by = c("Municipios" = "Id"))

namecol <- c('Municipality', 'Cluster5', 'Cluster6', 'Cluster7', 'Cluster8', 
             'Cluster9','Cluster10','Cluster11', 'Cluster12','Cluster13',
             'Cluster14','Cluster15','Cluster16','Cluster17','Cluster18',
             'Cluster19','Cluster20')

colnames(muni.new) <- namecol

muni.new$Cluster5 <- as.factor(muni.new$Cluster5)
muni.new$Cluster6 <- as.factor(muni.new$Cluster6)
muni.new$Cluster7 <- as.factor(muni.new$Cluster7)
muni.new$Cluster8 <- as.factor(muni.new$Cluster8)
muni.new$Cluster9 <- as.factor(muni.new$Cluster9)
muni.new$Cluster10 <- as.factor(muni.new$Cluster10)
muni.new$Cluster11 <- as.factor(muni.new$Cluster11)
muni.new$Cluster12 <- as.factor(muni.new$Cluster12)
muni.new$Cluster13 <- as.factor(muni.new$Cluster13)
muni.new$Cluster14 <- as.factor(muni.new$Cluster14)
muni.new$Cluster15 <- as.factor(muni.new$Cluster15)
muni.new$Cluster16 <- as.factor(muni.new$Cluster16)
muni.new$Cluster17 <- as.factor(muni.new$Cluster17)
muni.new$Cluster18 <- as.factor(muni.new$Cluster18)
muni.new$Cluster19 <- as.factor(muni.new$Cluster19)
muni.new$Cluster20 <- as.factor(muni.new$Cluster20)

str(muni.new)

muni.mult <- muni.new

tmi <- read.csv('TMI_var.csv',header=T,dec='.')
tmi <- tbl_df(tmi)
str(tmi)

muni.new <- left_join(muni.new, tmi, by = c('Municipality' = 'MPIOS'))

#muni.new <- inner_join(muni.new, test, by = c('Municipality' = 'Municipio'))
#rm(muni.new)
str(muni.new)
########################################################################
####### MAPA ###########################################################

muni <- readShapePoly("MAGNA_2012_MPIO_OK.shp")
depto <- readShapePoly("MAGNA_2012_DEPTO.shp")

att <- as.data.frame.matrix(muni@data)
att$MPIO_CCNCT <- as.integer(att$MPIO_CCNCT)

attrib <- left_join(att, muni.new, by = c('MPIO_CCNCT' = 'Municipality'))
muni@data <- attrib

write.table(attrib, file = 'grouping.csv', sep = ',')
str(attrib)
attach(attrib)

col<- brewer.pal(12,"Paired")
palette(col)


x11()
plot(muni, col = (muni$Cluster5), border = F, main = 'Mapa con 5 agrupamientos')
plot(depto, add= T)
box()

x11()
plot(muni, col = (muni$Cluster7), border = F, main = 'Mapa con 7 agrupamientos')
plot(depto, add= T)
box()

x11()
plot(muni, col = (muni$Cluster9), border = F, main = 'Mapa con 9 agrupamientos')
plot(depto, add= T)
box()

x11()
plot(muni, col = (muni$Cluster11), border = F, main = 'Mapa con 11 agrupamientos)')
plot(depto, add= T)
box()

incidencia<-muni.new$TMI2012
##########################################################################
display.brewer.all()
n <- 9
category <- classIntervals(muni$TMI2012, n, style = "jenks", na.ignore=T)
palette <- brewer.pal(n,"YlOrRd")
color <- findColours(category,(palette))
bins <- category$brks
lb <- length(bins)
x11()
plot(muni, col=color,border=T)
box()
legend("bottomleft",fill=palette,legend=(paste(round(bins[-length(bins)],1),"-",round(bins[-1],1))),cex=0.8, bg="white")

##########################################################################


#CIUDADES DE COLOMBIA

#library(maps)
#library(geosphere)

#x11()
#map("world", "Colombia")
#map.cities(country = "Colombia", pch=16, col= 'red')

##########################################################################
##########################################################################

#### ANÁLISIS DE VARIANZA ####

attach(attrib)

#par(mfrow=c(3,3))
x11()
boxplot(TMI2012 ~ Cluster5, notch=T, col = 'red', main = 'Box-Plot con 5 agrupamientos')
boxplot(TMI2012 ~ Cluster6, notch=T, col = 'red')
boxplot(TMI2012 ~ Cluster7, notch=T, col = 'red', main = 'Box-Plot con 7 agrupamientos')
boxplot(TMI2012 ~ Cluster8, notch=T, col = 'red')
boxplot(TMI2012 ~ Cluster9, notch=T, col = 'red', main = 'Box-Plot con 9 agrupamientos')
boxplot(TMI2012 ~ Cluster10, notch=T, col = 'red')
boxplot(TMI2012 ~ Cluster11, notch=T, col = 'red', main = 'Box-Plot con 11 agrupamientos')
boxplot(TMI2012 ~ Cluster12, notch=T, col = 'red')
boxplot(TMI2012 ~ Cluster13, notch=T, col = 'red')
boxplot(TMI2012 ~ Cluster14, notch=T, col = 'red')
boxplot(incidencia ~ Cluster15, notch=T, col = 'red')
boxplot(incidencia ~ Cluster16, notch=T, col = 'red')
boxplot(incidencia ~ Cluster17, notch=T, col = 'red')
boxplot(incidencia ~ Cluster18, notch=T, col = 'red')
boxplot(incidencia ~ Cluster19, notch=T, col = 'red')
boxplot(incidencia ~ Cluster20, notch=T, col = 'red')

vari<-TMI2012

x11()
hist(vari, 50, main = '', col='orange', ylab='TMI 2012')
plot(density(vari, na.rm=T), main = '')


#harmonic mean
1/mean(1/vari, na.rm=T)
median(vari, na.rm=T)
sd(vari, na.rm=T)
var(vari,na.rm=T)



attach(attrib)

clus.aov <- function(cluster){
a1 <- aov(vari ~ factor(cluster))
print(a1)
print(summary(a1))
print(x <- (sum((a1$res)^2)))
x11()
print(TukeyHSD(a1))
plot(TukeyHSD(a1))
}

clus.aov(Cluster5)
clus.aov(Cluster6)
clus.aov(Cluster7)
clus.aov(Cluster8)
clus.aov(Cluster9)
clus.aov(Cluster10)
clus.aov(Cluster11)
clus.aov(Cluster12)
clus.aov(Cluster13)
clus.aov(Cluster14)
clus.aov(Cluster15)
clus.aov(Cluster16)
clus.aov(Cluster17)
clus.aov(Cluster18)
clus.aov(Cluster19)
clus.aov(Cluster20)

anova5<-aov(TMI2012 ~ factor(Cluster5))

reg5<-lm(TMI2012 ~ factor(Cluster13)) 
summary(reg)
anova(reg)
confint(reg)
plant.mod = data.frame(Fitted = fitted(reg),Residuals = resid(reg), Treatment = Cluster5)
ggplot(plant.mod, aes(Fitted, Residuals, colour = Treatment)) + geom_point().

########################################################################
####### MULTINIVEL ###########################################################


tmi.v <- read.csv('TMI_var.csv',header=T,sep=',',dec='.')

muni.mult <- inner_join(muni.mult, tmi.v, by = c('Municipality' = 'MPIOS'))

str(muni.mult)

attach(muni.mult)

library(lme4) 

################### CON EL MODELO NULO

nullmodel5 <- lmer(TMI2012 ~ (1 | Cluster5), data = muni.mult, REML = FALSE) 
nullmodel6 <- lmer(TMI2012 ~ (1 | Cluster6), data = muni.mult, REML = FALSE) 
nullmodel7 <- lmer(TMI2012 ~ (1 | Cluster7), data = muni.mult, REML = FALSE) 
nullmodel8 <- lmer(TMI2012 ~ (1 | Cluster8), data = muni.mult, REML = FALSE) 
nullmodel9 <- lmer(TMI2012 ~ (1 | Cluster9), data = muni.mult, REML = FALSE) 
nullmodel10 <- lmer(TMI2012 ~ (1 | Cluster10), data = muni.mult, REML = FALSE) 
nullmodel11 <- lmer(TMI2012 ~ (1 | Cluster11), data = muni.mult, REML = FALSE) 
nullmodel12 <- lmer(TMI2012 ~ (1 | Cluster12), data = muni.mult, REML = FALSE) 
nullmodel13 <- lmer(TMI2012 ~ (1 | Cluster13), data = muni.mult, REML = FALSE) 
nullmodel14 <- lmer(TMI2012 ~ (1 | Cluster14), data = muni.mult, REML = FALSE) 
nullmodel15 <- lmer(TMI2012 ~ (1 | Cluster15), data = muni.mult, REML = FALSE) 
nullmodel16 <- lmer(TMI2012 ~ (1 | Cluster16), data = muni.mult, REML = FALSE) 
nullmodel17 <- lmer(TMI2012 ~ (1 | Cluster17), data = muni.mult, REML = FALSE) 
nullmodel18 <- lmer(TMI2012 ~ (1 | Cluster18), data = muni.mult, REML = FALSE) 
nullmodel19 <- lmer(TMI2012 ~ (1 | Cluster19), data = muni.mult, REML = FALSE) 
nullmodel20 <- lmer(TMI2012 ~ (1 | Cluster20), data = muni.mult, REML = FALSE) 

sum5<-summary(nullmodel5)
sum6<-summary(nullmodel6)
sum7<-summary(nullmodel7)
sum8<-summary(nullmodel7)
sum9<-summary(nullmodel9)
sum10<-summary(nullmodel10)
sum11<-summary(nullmodel11)
sum12<-summary(nullmodel12)
sum13<-summary(nullmodel13)
sum14<-summary(nullmodel14)
sum15<-summary(nullmodel15)
sum16<-summary(nullmodel16)
sum17<-summary(nullmodel17)
sum18<-summary(nullmodel18)
sum19<-summary(nullmodel19)
sum20<-summary(nullmodel20)

x11()
boxplot(sum5$res, sum6$res, sum7$res, sum8$res, sum9$res, sum10$res, sum11$res,
        sum12$res, sum13$res, sum14$res, sum15$res, sum16$res, sum17$res, sum18$res,
        sum19$res, sum20$res, notch=T)

D5<-deviance(nullmodel5)
D6<-deviance(nullmodel6)
D7<-deviance(nullmodel7)
D8<-deviance(nullmodel8)
D9<-deviance(nullmodel9)
D10<-deviance(nullmodel10)
D11<-deviance(nullmodel11)
D12<-deviance(nullmodel12)
D13<-deviance(nullmodel13)
D14<-deviance(nullmodel14)
D15<-deviance(nullmodel15)
D16<-deviance(nullmodel16)
D17<-deviance(nullmodel17)
D18<-deviance(nullmodel18)
D19<-deviance(nullmodel19)
D20<-deviance(nullmodel20)


deviances<-c(D5, D6, D7, D8, D9, D10, D11, D12, D13, D14, D15, D16, D17, D18, D19, D20)
plot(deviances, type='l', col='blue')

str(tmi.v)
str(muni.mult)

################### CON EL MODELO MULTINIVEL CON LAS VARIABLES DEL DANE

grupos <- c('5','6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20')

model5 <- lmer(TMI2012 ~ (1 | Cluster5) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model6 <- lmer(TMI2012 ~ (1 | Cluster6) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model7 <- lmer(TMI2012 ~ (1 | Cluster7) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model8 <- lmer(TMI2012 ~ (1 | Cluster8) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model9 <- lmer(TMI2012 ~ (1 | Cluster9) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model10 <- lmer(TMI2012 ~ (1 | Cluster10) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model11 <- lmer(TMI2012 ~ (1 | Cluster11) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model12 <- lmer(TMI2012 ~ (1 | Cluster12) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model13 <- lmer(TMI2012 ~ (1 | Cluster13) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model14 <- lmer(TMI2012 ~ (1 | Cluster14) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model15 <- lmer(TMI2012 ~ (1 | Cluster15) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model16 <- lmer(TMI2012 ~ (1 | Cluster16) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model17 <- lmer(TMI2012 ~ (1 | Cluster17) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model18 <- lmer(TMI2012 ~ (1 | Cluster18) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model19 <- lmer(TMI2012 ~ (1 | Cluster19) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 
model20 <- lmer(TMI2012 ~ (1 | Cluster20) + PROMFEC2012 + PROMBPN2012 + PROMCPRE2012 + PROMAPA2012 + PROMHNV2012 + PROEDAD2012 + PROEMB2012 + POBTOT_P2012 + PROMDPT10 + PROVAC10 + TAMUNCAT12 + PCRURA12 + TEMPERAT12, data = muni.mult, REML = FALSE) 

csum5<-summary(model5)
csum6<-summary(model6)
csum7<-summary(model7)
csum8<-summary(model7)
csum9<-summary(model9)
csum10<-summary(model10)
csum11<-summary(model11)
csum12<-summary(model12)
csum13<-summary(model13)
csum14<-summary(model14)
csum15<-summary(model15)
csum16<-summary(model16)
csum17<-summary(model17)
csum18<-summary(model18)
csum19<-summary(model19)
csum20<-summary(model20)

x11()
boxplot(csum5$res, csum6$res, csum7$res, csum8$res, csum9$res, csum10$res, csum11$res,
        csum12$res, csum13$res, csum14$res, csum15$res, csum16$res, csum17$res, csum18$res,
        csum19$res, csum20$res, notch=T)

cD5<-deviance(model5)
cD6<-deviance(model6)
cD7<-deviance(model7)
cD8<-deviance(model8)
cD9<-deviance(model9)
cD10<-deviance(model10)
cD11<-deviance(model11)
cD12<-deviance(model12)
cD13<-deviance(model13)
cD14<-deviance(model14)
cD15<-deviance(model15)
cD16<-deviance(model16)
cD17<-deviance(model17)
cD18<-deviance(model18)
cD19<-deviance(model19)
cD20<-deviance(model20)


deviances.null<-c(D5, D6, D7, D8, D9, D10, D11, D12, D13, D14, D15, D16, D17, D18, D19, D20)
deviances.comp<-c(cD5, cD6, cD7, cD8, cD9, cD10, cD11, cD12, cD13, cD14, cD15, cD16, cD17, cD18, cD19, cD20)
x11()
plot(deviances.null, type='l', col='blue', ylim=c(4500,8000))
lines(deviances.comp, col = 'red')


cAIC5<-AIC(model5)
cAIC6<-AIC(model6)
cAIC7<-AIC(model7)
cAIC8<-AIC(model8)
cAIC9<-AIC(model9)
cAIC10<-AIC(model10)
cAIC11<-AIC(model11)
cAIC12<-AIC(model12)
cAIC13<-AIC(model13)
cAIC14<-AIC(model14)
cAIC15<-AIC(model15)
cAIC16<-AIC(model16)
cAIC17<-AIC(model17)
cAIC18<-AIC(model18)
cAIC19<-AIC(model19)
cAIC20<-AIC(model20)

AIC.comp<-c(cAIC5, cAIC6, cAIC7, cAIC8, cAIC9, cAIC10, cAIC11, cAIC12, cAIC13, cAIC14, cAIC15, cAIC16, cAIC17, cAIC18, cAIC19, cAIC20)

x11()
plot(AIC.comp ~ grupos, type='b', col='red', lty = 1, lwd=2)
