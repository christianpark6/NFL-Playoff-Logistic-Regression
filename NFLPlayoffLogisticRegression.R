library(car)
library(alr3)
library(tidyverse)
library(dplyr)
library(stringr)
library(leaps)
library(ggplot2)
##Data cleaning
NFLTeams1 = cbind(NFC_AFC=0, NFLTeams1)
NFLTeams1$NFC_AFC[1:16] = 1
total_data <- merge(NFLTeams1,offensedata1,by="Tm")
par(mfrow=c(2,2))
plot(total_data[,c("Playoff",'PD','SRS','YdsP','AttP' )])
plot(total_data[,c("Playoff",'TDP','Scp','Cmp','AttR','FL')])
plot(total_data[,c("Playoff",'EXP','Int','FstDP','YdsR','Pen')])

colnames(total_data)
##Stepwise Forward Model Selection Process looking at AIC.
step(glm(Playoff=='1'~1, data=total_data), scope=list(lower=lm(W.L.~1, data=total_data) ,
upper=glm((Playoff=='1'~ PD +SRS + YdsP+TDP +Scp +Cmp +EXP + Int+FstDP+YdsR+Pen+FL+AttR+AttP), data=total_data), direction="forward" ))

mod1=  glm(Playoff == "1" ~ PD + Int + FL,data = total_data)
par(mfrow= c(2,2))
plot(mod1)
summary(mod1)
vif(mod1)

subs = regsubsets(Playoff==1 ~ PD +SRS + YdsP+TDP +Scp +Cmp +EXP + Int+FstDP+YdsR+Pen+FL+AttR+AttP, data=total_data)

summary(subs)

par(mfrow=c(1,1))
##Best Subsets Regression
subsets(subs, statistic="bic")
with(total_data, boxplot(SoS~as.factor(NFC_AFC)))
mod8= with(total_data, aov(SoS~as.factor(NFC_AFC)))
TukeyHSD(mod8)
mod2=  glm( Playoff=='1' ~ PD + Int ,data = total_data)
summary(mod2)
plot(mod2)
vif(mod2)
df= data.frame(PD=seq(0,250,5), Int=seq(0,50,1))
df$prob_playoff = predict(mod2, data.frame(PD=seq(0,250,5),Int=seq(0,50,1)))
df
plot(df)
summary(total_data$PD)
summary(total_data$Int)
par(mfrow= c(2,2))
plot(mod2)
summary(mod2)
par(mfrow=c(1,1))
with(total_data,plot(Playoff=='1'~ Int , col=ifelse(NFC_AFC==1,"blue","red")))
predW=predict(mod2, data.frame(Int = seq(0,50,1) , PD=seq(0,250,5)))
points(seq(0,50,1), predW, col="blue", type="l")
total_data$prob_playoff

df=data.frame(PD=c(seq(-200,200,1),seq(-200,200,1)), Playoff = c(rep("1",32),rep("0",32)))

total_data$prob_playoff = predict(mod2,total_data,type="response")
with(total_data[total_data$Playoff=="1",],plot(Int,prob_playoff,ylim=c(0, 1),lwd=2))
with(total_data[total_data$Playoff=="1",],lines(Int,prob_playoff,type="p"))

subs = regsubsets(W~ NFC_AFC +PF.x + PA+PD +MoV +SoS +SRS +OSRS+DSRS+G + PF.y + YdsT +Ply +YpP +TO+ FL +FstDP +Cmp +AttP +YdsP +TDP + Int+  NYpA+ FstDR +  AttR  +  YdsR+ TDR + YpA+ Pen+ YdsPen+FstPy+ Scp+ TOp, data=total_data)

summary(subs)

par(mfrow=c(1,1))

subsets(subs, statistic="bic")



mod2 = lm(W.L.~ YdsT + FstDR + AttR+TDR+Pen+Scp+PD+G, data=total_data)

summary(mod2)
vif(mod2)

mod3 = lm(W.L.~MoV + SoS, data= total_data)
summary(mod3)
plot(mod3)
par(mfrow=c(2,2))
plot(mod2)

par(mfrow=c(1,1))
with(total_data, interaction.plot(W, NFC_AFC, SRS))
with(total_data,boxplot(prob_playoff~NFC_AFC))
mod5= with(total_data,aov(prob_playoff~as.factor(NFC_AFC)))
TukeyHSD(mod5)
anova(mod5)

with(total_data, boxplot(W~NFC_AFC))
mod3=with(total_data, aov(W~as.factor(NFC_AFC)))
TukeyHSD(mod3)
mod4= with(total_data, aov(W~as.factor(NFC_AFC) + SRS + as.factor(NFC_AFC):SRS))
par(mfrow=c(2,2))
plot(mod4)
TukeyHSD(mod4)
summary(mod4)

mod4 = with(total_data, aov(W.L.~as.factor(NFC_AFC) + SRS + as.factor(NFC_AFC):SRS))
anova(mod4)
mod5 = with(total_data, aov(W.L.~as.factor(NFC_AFC) + SoS + as.factor(NFC_AFC):SoS))
anova(mod5)
with(total_data, interaction.plot(SoS, NFC_AFC, SRS))
with(total_data, interaction.plot(SoS, NFC_AFC, SRS))
mod6 = with(total_data, aov(SoS~as.factor(NFC_AFC) + Playoff + as.factor(NFC_AFC):Playoff))
anova(mod6)
mod7 = with(total_data, aov(Playoff~as.factor(NFC_AFC) + SoS + as.factor(NFC_AFC):SoS))
anova(mod7)
mod8 = with(total_data, glm(prob_playoff~ as.factor(NFC_AFC) + SoS + as.factor(NFC_AFC):SoS))
summary(mod8)
plot(mod8)
anova(mod8)
##Good Models Below
with(total_data, interaction.plot(prob_playoff, NFC_AFC, SoS))
mod9= with(total_data, glm(Playoff~as.factor(NFC_AFC) + SoS))
with(total_data, plot(prob_playoff~ SoS, col=ifelse(NFC_AFC==1 ,'blue' ,'red')))
summary(mod9)
with(total_data[total_data$NFC_AFC=="0",],plot(SoS,prob_playoff,ylim=c(0, 3),lwd=3,col='red'))
with(total_data[total_data$NFC_AFC=="1",],lines(SoS,prob_playoff,type="p", col='blue'))
summary(mod9)
par(mfrow=c(1,1))
with(total_data, interaction.plot(Playoff, NFC_AFC, SoS))

mod5 = with(total_data, aov(SRS~as.factor(NFC_AFC) + W.L. + as.factor(NFC_AFC):W.L.))

##Models for real world applicability (Looking at ways to decrease interceptions and increase point differential)
modPD=lm(PD~YdsP+TDP+TO+YdsPen+AttR, data = total_data)
summary(modPD)
modInt=lm(Int~YdsP+Cmp+FL+FstDP+AttP, data = total_data)
summary(modInt)

step(lm(PD~1, data=total_data), scope=list(lower=lm(PD~1, data=total_data), upper=lm((PD~YdsP+TDP+TO+YdsPen+AttR), data=total_data), direction="forward" ))
step(lm(Int~1, data=total_data), scope=list(lower=lm(Int~1, data=total_data), upper=lm((Int~YdsP+Cmp+FL+FstDP+AttP), data=total_data), direction="forward" ))

par(mfrow=c(1,1))
subsPD = regsubsets(PD~YdsP+TDP+TO+YdsPen+AttR, data=total_data)
subsInt = regsubsets(Int~YdsP+Cmp+FL+FstDP+AttP, data=total_data)
subsets(subsPD, statistic="bic")
subsets(subsInt, statistic="bic")
